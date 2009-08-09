function! ToggleTrueFalse()

python << EOF

# Replace 'true' with 'false' and vice-versa

import vim
import re

t = re.compile('true')
f = re.compile('false')

line = vim.current.line

if   t.search(line):
   vim.current.line = t.sub('false', line)
elif f.search(line):
   vim.current.line = f.sub('true', line)

#cb = vim.current.buffer
#row, col = vim.current.window.cursor
#print "row is ", row, " col is ", col
#vim.current.line="ww"

EOF
endfunction

