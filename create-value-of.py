import re


value_of_template = """

	(define value-of-{}
		(lambda ({})
			(cases {} {}
				
				{}

				(else )
									 )))	
"""

cases_template = """
				({} ({})
					...)
"""

def process_subtype(l):
	subtype = tokens[l + 1] 	
	r = match[l]
	s = l + 2	
	variables = []
	while s < r:
		variables.append(tokens[s + 1])
		s = match[s] + 1
	return cases_template.format(subtype, ' '.join(variables))	


def process_datatype(l):
	r = match[l]
	non_terminal = tokens[l + 2].lower()
	variable = non_terminal + "-var"
	
	cases = ""

	s = l + 4
	while s < r:
		case = process_subtype(s)	
		cases += case + "\n"
		s = match[s] + 1
	
	value_of  = value_of_template.format(non_terminal, variable, non_terminal, variable, cases)
	return value_of


f = open("interpreter.rkt", "r")
lines = f.readlines()
f.close()
lines = list(filter(lambda line: not line.strip().startswith(";"), lines))
all_text = ''.join(lines)

all_text = re.sub('\(', ' ( ', all_text)
all_text = re.sub('\)', ' ) ', all_text)
all_text = re.sub('\s+', ' ', all_text)
tokens = re.split(' ', all_text)
print(tokens)


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



