def template(opc, a, b, c, d):
  s = f"""#start_function
void main():
int-list: a, b, res
float-list:
  assign, a, {a}
  assign, b, {b}
  {opc}, res, {c}, {d}
  call, puti, res
#end_function"""
  return s

def bin_vv(opc, a, b):
  return template(opc, a, b, "a", "b")

def bin_vi(opc, a, b):
  return template(opc, a, b, "a", b)

def bin_iv(opc, a, b):
  return template(opc, a, b, a, "b")

def bin_ii(opc, a, b):
  return template(opc, a, b, a, b)

cases = [
  ("add", 20, 400, 420),
  ("add", 20, -5, 15),

  ("sub", 25, 5, 20),
  ("sub", 25, -5, 30),

  ("div", 36, 4, 9),
  ("div", 36, 5, 7),

  ("mult", 4, 9, 36),

  ("and", 39482, 23948,  23948 & 39482),
  ("or", 39482, 23948,  23948 | 39482)
]

templates = [bin_vv, bin_vi, bin_iv, bin_ii]
temp_names = ["vv", "vi", "iv", "ii"]

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
      f.write(str(expected))

    files.append(file_name)

with open ("testcase_names", "w") as f:
  for filename in files:
    f.write(filename + '\n')