import re


value_of_template = """
(define interpret-{}
	(lambda ({})
		(cases {} {} {}
			(else (displayln "ooops")))))	
"""

cases_template = """
			({} ({})
				(begin {}	
				))"""

cases_inner_template = """
					(interpret-{} {})"""

def process_subtype(l):
	subtype = tokens[l + 1] 	
	r = match[l]
	s = l + 2	
	variables = []
	types = []
	inner = ""
	real_cnt = 0
	while s < r:
		name = tokens[s + 1]
		var_type = tokens[s + 2][:-1]
		variables.append(name)
		if not var_type in ["symbol", "number"]:
				inner += cases_inner_template.format(tokens[s + 2][:-1], name)
				real_cnt += 1
		s = match[s] + 1
	
	if real_cnt == 0:
		inner = "\n					(void) " + inner
	return cases_template.format(subtype, ' '.join(variables), inner)	


def process_datatype(l):
	r = match[l]
	non_terminal = tokens[l + 2]
	variable = non_terminal.lower() + "-var"
	
	cases = ""

	s = l + 4
	while s < r:
		case = process_subtype(s)	
		cases += case + "\n"
		s = match[s] + 1
	
	value_of  = value_of_template.format(non_terminal, variable, non_terminal, variable, cases)
	return value_of


f = open("grammar-datatypes.rkt", "r")
lines = f.readlines()
f.close()
lines = list(filter(lambda line: not line.strip().startswith(";"), lines))
all_text = ''.join(lines)

all_text = re.sub('\(', ' ( ', all_text)
all_text = re.sub('\)', ' ) ', all_text)
all_text = re.sub('\s+', ' ', all_text)
tokens = re.split(' ', all_text)


stack = []
match = [-1 for _ in range(len(tokens))]



for i, token in enumerate(tokens):
	if token == "(":
		stack.append(i)
	elif token == ")":
		match[stack[-1]] = i
		match[i] = stack[-1]
		stack.pop()

for i, token in enumerate(tokens):
	if token == "(" and tokens[i + 1] == "define-datatype":
		x = process_datatype(i)
		print(x)

