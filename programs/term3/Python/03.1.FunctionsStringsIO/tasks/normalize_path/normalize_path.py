def normalize_path(path: str) -> str:
    """
    :param path: unix path to normalize
    :return: normalized path
    """
    # tokens: list[str] = [token for token in path.split('/') if token != '']
    tokens: list[str] = path.split('/')

    if len(path) == 0:
        return '.'

    if len(tokens) == 0:
        return path[0]

    stack: list[str] = []
    res = ''
    root = '/'

    if path[0] == '/':
        stack.append(root)
    else:
        stack.append('.')

    for token in tokens:
        if token == '' or token == '.':
            continue

        elif token == '..':
            if stack[-1] == '..' or stack[-1] == '.':
                stack.append('..')
            elif stack[-1] != root:
                stack.pop()
        else:
            stack.append(token)

    if len(stack) == 1:
        return stack[0]

    last_el = '' if len(stack) <= 1 else stack.pop()

    for el in stack:
        if el == '.':
            pass
        elif el == root:
            res += '/'
        else:
            res += el
            res += '/'

    res += last_el

    return res
