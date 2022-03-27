import os 

START = "#start_function"
MAIN = "void main():"
INT_LIST = "int-list:"
FLOAT_LIST = "float-list:"

def organize_programs_dir():
  fuzz_tests_dir = "./programs/fuzz_tests"
  os.mkdir(fuzz_tests_dir)
  
  fuzz_tests = ["fuzzed", "fuzzed2", "fuzzed3", "fuzzed4"]
  counter = 0
  for fuzz_test in fuzz_tests:
    old_test_dir_path = "programs/" + fuzz_test + "/" 
    for testfile in os.listdir(old_test_dir_path):
      old_testfile_path = old_test_dir_path + "/" + testfile 
      new_testfile_path = fuzz_tests_dir + "/fuzz" + str(counter) + ".ir"
      content = open(old_testfile_path, 'r').read()
      open(new_testfile_path, 'w').write(content)
      counter += 1

def generate_binop_tests():
  testfile_path = "programs/project2/instructions/binops.ir"
  
  
  
def main():
  organize_programs_dir()
        
if __name__ == "__main__":
  main()