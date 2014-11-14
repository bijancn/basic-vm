from math import factorial

bytecode_file = 'share/basic_test.bc'
N_factorials = 30
len_instructions = 5

with open(bytecode_file, 'w') as bcf:
  def write_list(lst):
    bcf.write(' '.join(map(str, lst)) + '\n')
  bcf.write('basic-vm-1.0\n')
  bcf.write("""These are 6 comment lines for illustration.

You might write anything here.
For example the invocation command and/or date.

N_factorials N_input_real N_tmp_real N_tmp_cmplx N_output_cmplx\n""")

  write_list([N_factorials, 1, 3, 1, 1])
  bcf.write('Factorial table\n')
  for i in range(N_factorials):
      bcf.write(str(factorial(i)) + '\n')
  bcf.write('Instructions\n')
  zeros = [str(i) for i in [0]*len_instructions]
  bcf.write(' '.join(zeros) + '\n')
  for i in range(N_factorials):
    write_list([1, 1, (-1)**i, i+1, 0])
    write_list([1, 2, 1, 2**i, 0])
    write_list([1, 3, i+1, 2**(i), 0])
    write_list([2, 1, 1, i, i+1])
    write_list(zeros)
  write_list([3,1,1,2,3])
  write_list(zeros)
