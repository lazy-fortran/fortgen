"""Exact Expr IR v1; ordinary SymPy adapter, independent of FortSym.

Only explicit True facts in the v1 vocabulary cross this boundary. Floating
expressions and unsupported assumptions require a separate lowering policy.
"""
from dataclasses import dataclass
import ast
import re

FACTS = ('real', 'positive', 'nonnegative', 'nonzero', 'integer',
         'positive_integer', 'zero', 'negative', 'nonpositive', 'rational',
         'algebraic')
FUNCTIONS = {'sin', 'cos', 'tan', 'exp', 'log', 'sqrt', 'Abs', 'sign', 'conjugate'}


@dataclass(frozen=True)
class Node:
    tag: str
    text: str = ''
    scope: str = ''
    facts: int = 0
    args: tuple = ()


def _quote(text):
    if not text.isascii():
        raise ValueError('Expr IR v1 requires ASCII text')
    return "'" + ''.join(f'\\x{ord(c):02x}' if ord(c) < 32 or c in "'\\" or
                         ord(c) > 126 else c for c in text) + "'"


def dumps(nodes):
    # Validate through the same public reader before emitting an artifact.
    rows = ['fortgen-expr-ir 1']
    for i, n in enumerate(nodes, 1):
        rows.append(f'{i} {n.tag} {_quote(n.text)} {_quote(n.scope)} {n.facts}' +
                    ''.join(f' {j}' for j in n.args))
    text = '\n'.join(rows) + '\n'
    loads(text)
    return text


def loads(text, *, max_nodes=10000):
    if len(text) > 16 * 1024 * 1024:
        raise ValueError('Expr IR byte budget exhausted')
    lines = text.splitlines()
    if not lines or lines[0] != 'fortgen-expr-ir 1':
        raise ValueError('unknown Expr IR version')
    nodes, symbols, depths = [], {}, []
    token = re.compile(r"'(?:\\x[0-9A-Fa-f]{2}|[^'\\])*'|[^ ]+")
    for line in lines[1:]:
        parts = token.findall(line)
        if len(parts) < 5 or len(parts) > 69 or len(nodes) >= max_nodes:
            raise ValueError('malformed row or resource budget exhausted')
        if int(parts[0]) != len(nodes) + 1:
            raise ValueError('invalid node index')
        text_value, scope = ast.literal_eval(parts[2]), ast.literal_eval(parts[3])
        if not isinstance(text_value, str) or not isinstance(scope, str):
            raise ValueError('text and scope must be quoted literals')
        if not text_value.isascii() or not scope.isascii() or len(text_value) > 65536:
            raise ValueError('unsupported text')
        facts, args = int(parts[4]), tuple(map(int, parts[5:]))
        if any(j < 1 or j > len(nodes) for j in args) or not 0 <= facts <= 2047:
            raise ValueError('invalid references or assumptions')
        tag = parts[1]
        valid = False
        if tag == 'integer':
            valid = not args and re.fullmatch(r'0|-?[1-9][0-9]*', text_value) is not None
        elif tag == 'rational':
            valid = not text_value and len(args) == 2
            if valid:
                valid = all(nodes[j-1].tag == 'integer' for j in args)
                valid = valid and nodes[args[1]-1].text != '0'
        elif tag == 'symbol':
            valid = bool(text_value) and not args
            key = scope, text_value
            if key in symbols and symbols[key] != facts:
                raise ValueError('conflicting symbol scope/assumptions')
            symbols[key] = facts
            positive = bool(facts & (2 | 32))
            nonnegative = bool(facts & 4) or positive or bool(facts & 64)
            nonpositive = bool(facts & (128 | 256 | 64))
            nonzero = bool(facts & (8 | 128)) or positive
            zero = bool(facts & 64) or nonnegative and nonpositive
            negative = bool(facts & 128)
            if positive and nonpositive:
                valid = False
            if negative and nonnegative or zero and nonzero:
                valid = False
        elif tag == 'constant':
            valid = not args and text_value in ('pi', 'E', 'I', 'oo', 'zoo', 'nan')
        elif tag in ('add', 'mul'):
            valid = not text_value and bool(args)
        elif tag == 'pow':
            valid = not text_value and len(args) == 2
        elif tag == 'function':
            valid = bool(text_value)
        if not valid or tag != 'symbol' and facts:
            raise ValueError('unsupported or malformed node')
        depth = 1 + max((depths[j-1] for j in args), default=0)
        if depth > 128:
            raise ValueError('Expr IR depth budget exhausted')
        depths.append(depth)
        nodes.append(Node(tag, text_value, scope, facts, args))
    return nodes


def from_sympy(expression, *, scope=''):
    import sympy as sp
    from sympy.core.function import AppliedUndef
    nodes, cache = [], {}

    def visit(e, depth=0):
        if depth >= 128 or len(nodes) >= 10000:
            raise ValueError('Expr IR traversal budget exhausted')
        if e in cache:
            return cache[e]
        args = tuple(visit(a, depth+1) for a in e.args)
        if isinstance(e, sp.Integer):
            n = Node('integer', str(e))
        elif isinstance(e, sp.Rational):
            n = Node('rational', args=(visit(sp.Integer(e.p)), visit(sp.Integer(e.q))))
        elif isinstance(e, sp.Symbol):
            facts = 0
            # Pinned SymPy 1.14 records explicit facts separately from inference.
            for name, value in e._assumptions_orig.items():
                if name == 'commutative' and value is True:
                    continue
                if name not in FACTS or value is not True:
                    raise ValueError('unsupported explicit assumption: ' + name)
                facts |= 1 << FACTS.index(name)
            n = Node('symbol', e.name, scope, facts)
        elif e in (sp.pi, sp.E, sp.I, sp.oo, sp.zoo, sp.nan):
            n = Node('constant', {sp.pi:'pi', sp.E:'E', sp.I:'I', sp.oo:'oo',
                                  sp.zoo:'zoo', sp.nan:'nan'}[e])
        elif isinstance(e, sp.Add):
            n = Node('add', args=args)
        elif isinstance(e, sp.Mul):
            n = Node('mul', args=args)
        elif isinstance(e, sp.Pow):
            n = Node('pow', args=args)
        elif isinstance(e, AppliedUndef) or e.func.__name__ in FUNCTIONS:
            n = Node('function', e.func.__name__, args=args)
        else:
            raise ValueError('unsupported exact expression: ' + type(e).__name__)
        nodes.append(n)
        cache[e] = len(nodes)
        return len(nodes)

    root = visit(expression)
    dumps(nodes)
    return nodes, root


def to_sympy(nodes, root=None):
    import sympy as sp
    loads(dumps(nodes))
    values = []
    for n in nodes:
        args = [values[j-1] for j in n.args]
        if n.tag == 'integer':
            value = sp.Integer(n.text)
        elif n.tag == 'rational':
            value = sp.Rational(*args)
        elif n.tag == 'constant':
            value = getattr(sp, n.text)
        elif n.tag == 'symbol':
            facts = {f: True for k, f in enumerate(FACTS) if n.facts & (1 << k)}
            if facts.pop('positive_integer', False):
                facts.update(positive=True, integer=True)
            name = n.text if n.scope == '' else f'@{len(n.scope)}:{n.scope}:{n.text}'
            value = sp.Symbol(name, **facts)
        elif n.tag == 'function':
            head = getattr(sp, n.text) if n.text in FUNCTIONS else sp.Function(n.text)
            value = head(*args)
        else:
            value = {'add':sp.Add, 'mul':sp.Mul, 'pow':sp.Pow}[n.tag](*args)
        values.append(value)
    return values[(root if root is not None else len(nodes))-1]
