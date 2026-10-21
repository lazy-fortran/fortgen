"""Independent ordinary-SymPy exact oracle for native and Python Expr IR."""
import subprocess
import sys
import tempfile
from pathlib import Path
try:
    import sympy as sp
except ImportError:
    raise SystemExit(77)
from fortgen.expr_ir import loads, dumps, from_sympy, to_sympy, Node

with tempfile.TemporaryDirectory() as tmp:
    path = Path(tmp) / 'native.expr'
    subprocess.run([sys.argv[1], str(path)], check=True, capture_output=True, text=True)
    nodes = loads(path.read_text())
    expression = to_sympy(nodes)
    x = sp.Symbol("@5:local:x'\\n", real=True, positive=True)
    assert sp.simplify(expression - sp.Rational(9007199254740993, 10)*x) == 0
    assert expression.coeff(x) * 10 == 9007199254740993
    assert dumps(loads(dumps(nodes))) == dumps(nodes)

x = sp.Symbol('x', positive=True)
original = (x + sp.Rational(1, 10))**3 + sp.Integer(9007199254740993)
nodes, root = from_sympy(original)
assert sp.simplify(to_sympy(nodes, root) - original) == 0
assert to_sympy(nodes, root).subs(x, 2) == sp.Rational(9007199254741002261, 1000)
for bad in ('fortgen-expr-ir 2\n',
            "fortgen-expr-ir 1\n1 integer '01' '' 0\n",
            "fortgen-expr-ir 1\n1 pow '' '' 0 1 2\n",
            "fortgen-expr-ir 1\n1 symbol 'x' '' 96\n"):
    try:
        loads(bad)
    except ValueError:
        pass
    else:
        raise AssertionError('malformed or contradictory input accepted')
try:
    from_sympy(sp.Symbol('z', finite=True))
except ValueError:
    pass
else:
    raise AssertionError('unsupported assumption dropped')
assert 'fortsym' not in sys.modules
print('independent exact Expr IR adapter PASS')
