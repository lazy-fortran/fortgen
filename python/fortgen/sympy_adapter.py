"""Direct SymPy -> FortGen kernel-IR adapter.

The adapter owns no symbolic algebra. SymPy constructs/simplifies expressions;
this module only lowers the supported scalar expression tree into the stable
fortgen.kernel_ir.v1 interchange consumed by the Fortran FortGen backend.
"""

from __future__ import annotations

from dataclasses import dataclass
import os
import shlex
import subprocess
import tempfile
from pathlib import Path
from typing import Mapping, Sequence


_FUNCTIONS = {
    "sin": "sin", "cos": "cos", "tan": "tan",
    "asin": "asin", "acos": "acos", "atan": "atan", "atan2": "atan2",
    "sinh": "sinh", "cosh": "cosh", "tanh": "tanh",
    "asinh": "asinh", "acosh": "acosh", "atanh": "atanh",
    "exp": "exp", "log": "log", "Abs": "abs",
    "erf": "erf", "erfc": "erfc", "gamma": "gamma", "loggamma": "loggamma",
    "besselj": "besselj", "bessely": "bessely",
    "besseli": "besseli", "besselk": "besselk",
    "Min": "Min", "Max": "Max",
}


@dataclass(frozen=True)
class _Node:
    kind: str
    operands: tuple[int, ...] = ()
    value: float | None = None
    name: str | None = None


class Kernel:
    def __init__(self, outputs, *, args=None, name="kernel", precision="real64"):
        import sympy as sp

        if isinstance(outputs, Mapping):
            pairs = list(outputs.items())
        else:
            pairs = [("result", sp.sympify(outputs))]
        if not pairs:
            raise ValueError("at least one output is required")
        self.name = str(name)
        self.precision = str(precision)
        self.outputs = [(str(k), sp.sympify(v)) for k, v in pairs]
        if args is None:
            symbols = set()
            for _, expr in self.outputs:
                symbols.update(expr.free_symbols)
            self.args = sorted(symbols, key=lambda s: str(s))
        else:
            self.args = [sp.sympify(a) for a in args]
            if not all(isinstance(a, sp.Symbol) for a in self.args):
                raise TypeError("args must contain SymPy Symbols")
        self._arg_names = {str(a) for a in self.args}
        if len(self._arg_names) != len(self.args):
            raise ValueError("argument names must be unique")

    def _lower(self):
        import sympy as sp

        nodes: list[_Node] = []
        cache = {}

        def add(node: _Node):
            nodes.append(node)
            return len(nodes)

        def visit(expr):
            expr = sp.sympify(expr)
            if expr in cache:
                return cache[expr]

            if isinstance(expr, sp.Symbol):
                if str(expr) not in self._arg_names:
                    raise ValueError(f"free symbol {expr} is not an input argument")
                idx = add(_Node("symbol", name=str(expr)))
            elif expr == sp.pi:
                idx = add(_Node("constant", name="pi"))
            elif expr == sp.E:
                idx = add(_Node("constant", name="e"))
            elif expr.is_Number:
                value = float(expr)
                if not (float("-inf") < value < float("inf")):
                    raise ValueError(f"non-finite numeric literal: {expr}")
                idx = add(_Node("literal", value=value))
            elif isinstance(expr, sp.Add):
                idx = add(_Node("add", tuple(visit(a) for a in expr.args)))
            elif isinstance(expr, sp.Mul):
                idx = add(_Node("mul", tuple(visit(a) for a in expr.args)))
            elif isinstance(expr, sp.Pow):
                idx = add(_Node("pow", (visit(expr.base), visit(expr.exp))))
            else:
                fname = expr.func.__name__
                canonical = _FUNCTIONS.get(fname)
                if canonical is None:
                    raise NotImplementedError(
                        f"SymPy function {fname} is outside fortgen.kernel_ir.v1"
                    )
                idx = add(_Node("function", tuple(visit(a) for a in expr.args),
                                    name=canonical))
            cache[expr] = idx
            return idx

        roots = [(name, visit(expr)) for name, expr in self.outputs]
        return nodes, roots

    def to_ir(self, *, target="fortran_cpu"):
        nodes, roots = self._lower()
        lines = [
            "fortgen.kernel_ir.v1",
            f"kernel {self.name}",
            f"precision {self.precision}",
            f"target {target}",
        ]
        lines.extend(f"arg {a}" for a in self.args)
        lines.extend(f"out {name} {root}" for name, root in roots)
        for i, node in enumerate(nodes, 1):
            if node.kind == "literal":
                lines.append(f"node {i} literal {node.value:.17e}")
            elif node.kind in {"symbol", "constant"}:
                lines.append(f"node {i} {node.kind} {node.name}")
            elif node.kind == "function":
                ops = " ".join(str(x) for x in node.operands)
                lines.append(
                    f"node {i} function {node.name} {len(node.operands)}"
                    + (f" {ops}" if ops else "")
                )
            else:
                ops = " ".join(str(x) for x in node.operands)
                lines.append(
                    f"node {i} {node.kind} {len(node.operands)}"
                    + (f" {ops}" if ops else "")
                )
        lines.append("end")
        return "\n".join(lines) + "\n"

    def emit(self, backend="fortran"):
        if backend not in {"fortran", "cuda"}:
            raise ValueError("backend must be 'fortran' or 'cuda'")
        target = "cuda" if backend == "cuda" else "fortran_cpu"
        command = shlex.split(os.environ.get("FORTGEN_CODEGEN", "fortgen-codegen"))
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "kernel.fgir"
            path.write_text(self.to_ir(target=target), encoding="utf-8")
            proc = subprocess.run(
                [*command, backend, str(path)],
                check=False, text=True, capture_output=True
            )
        if proc.returncode:
            raise RuntimeError(proc.stderr.strip() or "fortgen-codegen failed")
        return proc.stdout

    def emit_fortran(self):
        return self.emit("fortran")

    def emit_cuda(self):
        return self.emit("cuda")


def kernel(outputs, *, args=None, name="kernel", precision="real64"):
    return Kernel(outputs, args=args, name=name, precision=precision)
