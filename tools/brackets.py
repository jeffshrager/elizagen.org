#!/usr/bin/python3

def convert_brackets(i, o):
    stack = []
    line = 1    
    while True:
        c = i.read(1)
        if c == '':
            break
        elif c in ('(', '['):
            stack.append(c)
            o.write('(')
        elif c == ')':
            if stack == [] or stack.pop() != '(':
                raise RuntimeError('Unbalanced ")" on line %d' % line)
            o.write(')')
        elif c == ']':
            if '[' not in stack:
                raise RuntimeError('Unbalanced "]" on line %d' % line)
            while stack != [] and stack.pop() == '(':
                o.write(')')
            o.write(')')
        elif c == '\n':
            line += 1
            o.write(c)
        else:
            o.write(c)
    if stack != []:
        raise RuntimeError('Unbalanced %s' % (stack,))


if __name__ == '__main__':
    import sys
    convert_brackets(sys.stdin, sys.stdout)
