import sys
import pprint
import itertools
import string
import subprocess

DEBUG = True
SPECIAL_CHARS = ['\\', ':', '.', '=', '$', '-', '(', ')', '+', '*']

def print_debug(obj):
    if DEBUG:
        print(str(obj))

def exit_with_error(string):
    print(string, file=sys.stderr)
    sys.exit(1)


def contains_unescaped(string, character):
    i = 0
    while i < len(string):
        if string[i] == character:
            return True
        if string[i] == '\\':
            i += 1
        i += 1
    return False


def unescape(string):
    result = ''
    escaped = False
    for c in string:
        assert c in SPECIAL_CHARS or not escaped
        if c == '\\' and not escaped:
            escaped = True
        else:  # Normal character or escaped SPECIAL_CHARS
            result += c
            escaped = False
    assert not escaped
    return result


def escape(string):
    for char in SPECIAL_CHARS:
        string = string.replace(char, '\\' + char)
    return string


def tokenize(string, split_chars, remove_split_chars=True):
    #special_chars_without_split = [x for x in SPECIAL_CHARS if x not in split_chars]
    result = []
    temp = ''
    escaped = False
    for c in string:
        if c == '\\' and not escaped:
            escaped = True
        #elif c in special_chars_without_split and not escaped:
        #    exit_with_error('Found %s unescaped in %s' % (c, string))
        elif c not in SPECIAL_CHARS and escaped:
            exit_with_error('Found normal character %s escaped in %s' % (c, string))
        elif c in SPECIAL_CHARS and escaped:
            temp += '\\' + c
            escaped = False
        else:  # Normal character or in split_chars
            if c in split_chars:
                result.append(temp)
                temp = ''
                if not remove_split_chars:
                    temp += c
            else:
                temp += c
            escaped = False
    if escaped:
        exit_with_error('Found \\ at end of %s' % string)
    result.append(temp)
    return result

def parse_tokens(string):
    tokens = tokenize(string, [' '])
    tokens = list(itertools.chain.from_iterable([tokenize(t, ['+', '*'], remove_split_chars=False) for t in tokens]))
    #if len(tokens) > 1 and tokens[0] == '':  # TODO is this a good fix?
    if len(tokens) >= 1 and tokens[0] == '':  # TODO is this a good fix?
        return tokens[1:]
    return tokens

def parse_pattern(string):  # '(' { [ '+' ] <token> } [ '*' <token> ] ')'
    print_debug("parse_pattern " + string)
    assert len(string) >= 2  # Because of the parentheses
    assert '(' == string[0]
    assert ')' == string[-1]
    return parse_tokens(string[1:-1])


def parse_patterns(string):  # { <pattern> }
    print_debug("parse_patterns " + string)
    parts = tokenize(string, ['(', ')'], remove_split_chars=False)
    print_debug("parse_patterns parts " + str(parts))
    if len(parts) % 2 != 1 or parts[0] != '':
        exit_with_error('Cannot split up parentheses in patterns of %s' % string)
    i = 1
    result = []
    while i < len(parts):
        combined_part = parts[i] + parts[i + 1]
        if len(combined_part) < 2 or combined_part[0] != '(' or combined_part[-1] != ')':
            exit_with_error('Cannot find corresponding parentheses in patterns of %s' % string)
        print_debug("parse_patterns combined_part " + str(combined_part))
        result.append(parse_pattern(combined_part))
        i += 2
    return result


def parse_atom1(string):  # <name> { <pattern> }
    print_debug("parse_atom1 " + string)
    name_list = tokenize(string, ['('], remove_split_chars=False)
    assert len(name_list) > 0  # At least [''] must be in name_list
    name = name_list[0]
    print_debug("parse_atom1 name " + name)
    print_debug("parse_atom1 list " + str(name_list[1:]))
    patterns = parse_patterns(''.join(name_list[1:]))
    return name, patterns


def parse_atom(string):  # <name> { <pattern> } '-' { <pattern> }
    print_debug("parse_atom " + string)
    if not contains_unescaped(string, '-'):
        exit_with_error('Cannot find unescaped - in atom %s' % string)
    parts = tokenize(string, ['-'])
    if len(parts) != 2:
        exit_with_error('No single - in atom %s' % string)
    name, patterns1 = parse_atom1(parts[0])
    patterns2 = parse_patterns(parts[1])
    return name, patterns1, patterns2


def parse_shell_goal(string):  # '$' <pattern> <pattern> '-' <pattern> <pattern>
    print_debug("parse_shell_goal " + string)
    assert string[0] == '$'
    parts = tokenize(string[1:], ['-'])
    if len(parts) != 2:
        exit_with_error('Cannot find unescaped - in shell goal %s' % string)
    patterns1 = parse_patterns(parts[0])
    patterns2 = parse_patterns(parts[1])
    if len(patterns1) != 2 or len(patterns2) != 2:
        exit_with_error('There are not exactly 4 patterns in shell goal %s' % string)
    return patterns1[0], patterns1[1], patterns2[0], patterns2[1]


def parse_equals(string):  # <pattern> '=' <pattern>
    print_debug("parse_equals " + string)
    parts = tokenize(string, ['='])
    assert len(parts) != 1  # Because of parse_goal() checking it
    if len(parts) > 2:
        exit_with_error('Equality goal %s has multiple = ' % string)
    return parse_pattern(parts[0]), parse_pattern(parts[1])


def parse_goal(string):  # <atom> | <pattern> '=' <pattern> | '$' <pattern> <pattern> '-' <pattern> <pattern>
    print_debug("parse_goal " + string)
    if len(string) < 1:
        exit_with_error('Cannot parse zero length goal %s' % string)
    if string[0] == '$':
        return parse_shell_goal(string)
    elif contains_unescaped(string, '='):
        return parse_equals(string)
    return parse_atom(string)


def parse_rule(string):  # <head> <body> '.'  # '.' checked by parse_program()
    print_debug("parse_rule " + string)
    head_or_body = [x for x in tokenize(string, [':'])]
    assert len(head_or_body) > 0
    head = head_or_body[0]
    goals = head_or_body[1:] if len(head_or_body) > 1 else []
    return parse_atom(head), [parse_goal(x) for x in goals]


def parse_program(string):  # { <rule> }  # Checks '.' too
    print_debug("parse_program " + string)
    string = string.replace('\n', '').replace('\r', '')  # TODO Remove newlines?
    if string[-1] != '.':
        exit_with_error('Last . missing of program %s' % string)
    return [parse_rule(rule) for rule in tokenize(string, ['.'])[:-1]]


#######################################################################################################################
#  Interpreter Part                                                                                                   #
#######################################################################################################################


def is_full_pattern(pattern, variables):
    return all(map(lambda token: token in variables or token == '' or token[0] not in ['+', '*'], pattern))


def get_value(token, variables):
    if len(token) == 0:
        return ''
    if token[0] in ['+', '*']:
        return variables.get(token)
    return token  # literal token


def get_type(token):
    if len(token) == 0 or token[0] not in ['+', '*']:
        return ''
    return token[0]


def variable_generator():
    current = 0
    while True:
        yield '__var_%s' % current
        current += 1


def pattern_length_without_star(pattern):
    i = 0
    for token in pattern:
        if len(token) > 0 and token[0] == '*':
            break
        i += 1
    return i


def matches(pattern1, pattern2, variables):
    if not is_full_pattern(pattern2, variables):
        return False
    print_debug('Matching ' + str(pattern1) + ' # ' + str(pattern2))
    #if len(pattern_to_string(pattern2, variables)) == 0 and len(pattern1) >= 2:  # +x*x does not match ''
    #    return False
    # TODO +x+y*z does not match 'a '
    """if len(pattern2) == 0:
        if len(pattern1) > 0:
            if get_type(pattern1[0]) == '*':  # Only *z matches []
                variables[pattern1[0]] = ''
                return True
            return False
        return True"""
    m = pattern_length_without_star(pattern1)
    n = pattern_length_without_star(pattern2)
    new_variables = variables.copy()
    for i in range(min(m, n)):  # For all min(m, n)
        value1 = get_value(pattern1[i], new_variables)
        value2 = get_value(pattern2[i], new_variables)
        if value1:  # If ai is initialized
            if value1 != value2:  # then ai and ci stand for the same token
                return False
        else:  # if ai is uninitialized
            new_variables[pattern1[i]] = value2  # ai becomes initialized with the token that ci stands for
    if n > m:
        if len(pattern1) > m and pattern1[m] not in new_variables.keys():  # there is an uninitialized b
            tokens = []
            for i in range(m, n):
                value2 = get_value(pattern2[i], new_variables)
                tokens.append(value2)
            new_variables[pattern1[m]] = ' '.join(tokens)  # then b becomes initialized
        else:
            return False
    if m > n:
        if len(pattern2) > n:  # If m > n and d is specified
            value_d = get_value(pattern2[n], new_variables)
            if value_d == '':
                return False
            d = value_d.split(' ')
            for i in range(n, m):  # From here similar to first case
                value1 = get_value(pattern1[i], new_variables)
                if len(d) == i - n:  # There are not enough d parts (not defined in the assignment!)
                    return False
                d_part = d[i - n]
                if value1:
                    if value1 != d_part:
                        return False
                else:
                    new_variables[pattern1[i]] = d_part
            if len(pattern1) > m:  # if b is specified
                if pattern1[m] not in new_variables.keys():
                    new_variables[pattern1[m]] = ' '.join(d[m - n:])  # b is initialized with the remainder of the character sequence in d after removing the already used m − n tokens
                else:  # b is specified and initialized, this part is not defined in the assignment!
                    if get_value(pattern1[m], new_variables).split(' ') != d[m - n:]:  # Check it ?!
                        return False
            else:
                if len(d) != m - n:  # d contains just the m − n already used tokens and then ends
                    return False
        else:
            return False
    if m == n:
        if len(pattern1) > m and len(pattern2) > m:  # when there are b and d and m==n (not defined in the assignment!)
            if pattern1[m] not in new_variables.keys():
                new_variables[pattern1[m]] = get_value(pattern2[n], new_variables)
            else:
                if get_value(pattern1[m], new_variables) != get_value(pattern2[n], new_variables):
                    return False
        elif len(pattern1) > m:
            if get_type(pattern1[m]) == '*':  # Only *z matches []
                new_variables[pattern1[m]] = ''
            else:
                return False
        elif len(pattern2) > m:
            if len(get_value(pattern2[m], new_variables)) != 0:
                return False
    variables.update(new_variables)
    return True


def execute_equal_goal(goal, variables):
    return matches(goal[0], goal[1], variables)  # is_full_pattern() will be checked in matches()


def copy_pattern(pattern, vg, replaced_variables):
    result = []
    for t in pattern:
        if get_type(t) != '':
            if t in replaced_variables.keys():
                new = replaced_variables[t]
            else:
                new = get_type(t) + next(vg)
                replaced_variables[t] = new
            result.append(new)
        else:
            result.append(t)
    return result


def copy_patterns(patterns, vg, replaced_variables):
    return [copy_pattern(p, vg, replaced_variables) for p in patterns]


def copy_goal(goal, vg, replaced_variables):
    if len(goal) == 3:  # Tuple with 3 elements is an atom
        return goal[0], copy_patterns(goal[1], vg, replaced_variables), copy_patterns(goal[2], vg, replaced_variables)
    elif len(goal) == 2:  # Tuple with 2 elements is an equals goal
        return tuple([copy_pattern(p, vg, replaced_variables) for p in goal])
    else:  # Tuple with 4 elements is a shell goal
        assert len(goal) == 4
        return tuple([copy_pattern(p, vg, replaced_variables) for p in goal])


def execute_atom_goal(goal, rule, variables, vg):
    new_goals = []
    head, body = rule[0], rule[1]
    if goal[0] != head[0]:  # Check for x (name in the head)
        return None
    if not all([is_full_pattern(p, variables) for p in goal[1]]):  # Check if goal first half is full
        return None
    if len(head[1]) != len(goal[1]) or len(head[2]) != len(goal[2]):
        return None
    replaced_variables = {}
    head = copy_goal(head, vg, replaced_variables)
    body = [copy_goal(b, vg, replaced_variables) for b in body]
    print_debug(replaced_variables)
    for g1, g2 in zip(head[1], goal[1]):
        if not matches(g1, g2, variables):
            return None
    #for q, p in zip(head[2], goal[2]):
    for q, p in zip(goal[2], head[2]):
        new_goals.append((q, p))
    new_goals.extend(body)
    return new_goals


def pattern_to_string(pattern, variables):
    return ' '.join([unescape(get_value(t, variables)) for t in pattern])


def execute_shell_goal(goal, variables):
    if not is_full_pattern(goal[0], variables):
        return None
    if not is_full_pattern(goal[1], variables):
        return None
    command = pattern_to_string(goal[0], variables)
    print_debug('cmd: ' + command)
    stdin = pattern_to_string(goal[1], variables)
    process = subprocess.run(command, shell=True, input=stdin, capture_output=True, text=True)
    output = process.stdout if process.stdout != '' else process.stderr
    output = output[:-1] if len(output) > 0 and output[-1] == '\n' else output
    output = escape(output.replace('\n', ' '))  # TODO check if good solution
    returncode = str(process.returncode)

    return [(goal[2], parse_tokens(output)), (goal[3], parse_tokens(returncode))]


def execute_program(program, goals):
    variables = {}
    vg = variable_generator()
    while len(goals) > 0:
        for goal in goals:
            if len(goal) == 3:  # Tuple with 3 elements is an atom
                new_goals = None
                for rule in program:
                    new_goals = execute_atom_goal(goal, rule, variables, vg)
                    if new_goals is not None:  # Otherwise [] is false
                        print_debug('Executed atom r/g: ' + str(rule) + '  ##### ' + str(goal))
                        goals.extend(new_goals)
                        goals.remove(goal)
                        break
                if new_goals:  # Because of the .remove() we restart loop
                    break
            elif len(goal) == 2:  # Tuple with 2 elements is an equals goal
                if execute_equal_goal(goal, variables):
                    print_debug('Executed equal g: ' + str(goal))
                    goals.remove(goal)
                    break
            else:  # Tuple with 4 elements is a shell goal
                assert len(goal) == 4
                new_goals = execute_shell_goal(goal, variables)
                if new_goals is not None:  # Otherwise [] is false
                    print_debug('Executed shell g: ' + str(goal))
                    goals.extend(new_goals)
                    goals.remove(goal)
                    break


def main():
    if len(sys.argv) < 3:
        exit_with_error('Usage: python %s <filename>.grl goal {goal }' % sys.argv[0])
    else:
        with open(sys.argv[1]) as f:
            program = parse_program(f.read())
        if DEBUG:
            pprint.pprint(program, width=1)

        goals = []
        for i in range(2, len(sys.argv)):
            goals.append(parse_goal(sys.argv[i]))
        if DEBUG:
            pprint.pprint(goals, width=1)

        print_debug('Start executing')
        execute_program(program, goals)


if __name__ == "__main__":
    main()

