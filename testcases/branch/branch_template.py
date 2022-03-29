# expected: t for taken, n for not taken
def template(opc, a, b, c, d):
    s = f"""#start_function
void main():
int-list: a, b
float-list:
  assign, a, {a}
  assign, b, {b}
  {opc}, branch_taken, {c}, {d}
not_taken:
  call, putc, 110
  goto, end
branch_taken:
  call, putc, 116
end:
#end_function"""
    return s

def br_vv(opc, a, b):
  return template(opc, a, b, "a", "b")

def br_vi(opc, a, b):
  return template(opc, a, b, "a", b)

def br_iv(opc, a, b):
  return template(opc, a, b, a, "b")

def br_ii(opc, a, b):
  return template(opc, a, b, a, b)


taken = True
not_taken = False

templates = [br_vv, br_vi, br_iv, br_ii]
temp_names = ["vv", "vi", "iv", "ii"]

cases = [
  ("breq", 5, 5, taken),
  ("breq", 6, 5, not_taken),
 
  ("brneq", 6, 5, taken),
  ("brneq", 5, 5, not_taken),

  ("brlt", 5, 420, taken),
  ("brlt", 5, 5, not_taken),


  ("brgt", 5, 0, taken),
  ("brgt", 5, 5, not_taken),
  ("brgt", 5, 6, not_taken),

  ("brgeq", 5, 5, taken),
  ("brgeq", 5, 6, not_taken),

  ("brleq", 5, 5, taken),
  ("brleq", 3, 5, taken),
  ("brleq", 5, 3, not_taken)
]

files = []
for i, (op, a, b, expected) in enumerate(cases):
  for tmpl, tmpl_name in zip(templates, temp_names):
    file_name = f"{op}_{tmpl_name}_{i}"
    ir_file = f"{file_name}.ir"
    output = f"{file_name}.out"
    with open(ir_file, "w") as f:
      res = tmpl(op, a, b)
      f.write(res)

    with open(output, "w") as f:
      if expected:
        f.write("t")
      else:
        f.write("n")

    files.append(file_name)

with open ("testcase_names", "w") as f:
  for filename in files:
    f.write(filename + '\n')