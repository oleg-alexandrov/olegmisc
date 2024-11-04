#!/usr/bin/env python

# Replace the function arguments in the .cc file with the ones from the .h file
# which presumably is more up-to-date. 

import os, re, sys, textwrap, glob

def findBalancedBlock(a):
    """
    Split into two blocks, so that in the first block the parentheses are balanced.
    This should ignore any text in comments without wiping it. 
    """
    # TODO(oalexan1): This does not handle well an argument like: "https://www.google.com",
    # as it is seen as a comment. Below have a temporary workaround.
    count = 0
    num = len(a)
    i = 0
    while i < num:

        if a[i] == '/' and (i == 0 or a[i - 1] != ':'): # workaround
            if i + 1 < num and a[i + 1] == '/':
                # Skip the comment
                while i < num and a[i] != '\n':
                   i = i + 1
            # Go forward either way
            i = i + 1
            continue
        
        if a[i] == '(':
            count = count + 1
            i = i + 1
            continue
            
        if a[i] == ')':
            count = count - 1
            i = i + 1
            if count == 0:
                break
            continue

        i = i + 1

    before = a[:i]
    after = a[i:]
    
    return (before, after)

# Split text into comments and non-comments. Comments start with // and go to
# the end of the line. Put all resulting blocks in a list in the order they
# appear in the text.
def split_by_comments(text):
    blocks = []
    block = ''

    # Unlikely text to serve as a separator
    sep = ' xxOddSeparatorxx '
    
    # Given a comment, put a separator before it starts and at the end of the line
    text = re.sub(r'(//.*?)(\n|$)', sep + r'\1' + r'\2' + sep, text)
    
    # Split by the separator
    blocks = text.split(sep)
    
    return blocks

# Find if a given string has a balanced number of parentheses, brackets, angles, and braces.
def is_balanced(s):
    stack = []
    for c in s:
        if c in '([{<':
            stack.append(c)
        elif c in ')]}>':
            if not stack:
                return False
            if c == ')' and stack[-1] != '(':
                return False
            if c == ']' and stack[-1] != '[':
                return False
            if c == '}' and stack[-1] != '{':
                return False
            if c == '>' and stack[-1] != '<':
                return False
            stack.pop()
    return not stack

# Remove default values for function arguments
def removeDefaultArgs(args):
    num = len(args)
    for i in range(num):
        m = re.match(r'^(.*?)(\s*=\s*.*?)(,\s*$|$)', args[i], re.DOTALL)
        if m:
            args[i] = m.group(1) + m.group(3)
    
    return args
    
def balancedSplitByComma(args):
    """
       Split by comma in such a way that each resulting entity has a balanced number
       of parentheses, brackets, angles, and braces.
    """
         
    # Swap comma and newline as we want to keep each newline together with the preceding
    # text.
    args = args.replace(',\n', '\n,')
    
    bargs = []
        
    # Split the arguments by comma
    args = args.split(',')
    
    # make each arg balanced
    num = len(args)
    
    i = 0
    while True:
        # Stop if processed all arguments
        if i >= num:
            break
            
        # Append tokens till balanced
        arg = ''
        isBalanced = False
        for j in range(i, num):
            if arg != "":
                arg = arg + ',' # put back the comma
            arg = arg + args[j]
            if is_balanced(arg):
                bargs += [arg + ','] # put back the comma
                i = j + 1
                break
    
    # Wipe the comma from the last argument
    if len(bargs) > 0 and len(bargs[-1]) > 0:
        bargs[-1] = bargs[-1][:-1]
    
    # Swap back the comma and newline
    for i in range(len(bargs)):
        bargs[i] = bargs[i].replace('\n,', ',\n')
        
    return bargs

def rmDefaultInArgStr(args):
    """
    Remove default values for arguments. First split by comma with balanced parentheses.
    Then wipe the default values.
    """
    
    # Separate the starting and ending parentheses
    m = re.match(r'^(\()(.*)(\).*?)$', args, re.DOTALL)
    if not m:
      raise Exception("Expecting a string having parentheses.")
    
    beg = m.group(1)
    mid = m.group(2)
    end = m.group(3)
    
    mid = balancedSplitByComma(mid)
    mid = removeDefaultArgs(mid)
    
    # Join back into a string
    mid = ''.join(mid)
    
    # Append back the parentheses
    args = beg + mid + end
    
    return args
    
def splitByEmptyLines(text):
    """
    Split by text by a an empty line that may have some spaces.
    """

    # First replace a newline followed by spaces followed by newline
    # with just two newlines. Match across multiple lines.
    text = re.sub(r'\n\s*\n', '\n\n', text, re.DOTALL)

    # Now split by two newlines
    blocks = text.split('\n\n')

    return blocks   

def splitByTopComments(text):
    """
    Return the top-most lines having comments, then the rest of the text.
    """

    comments = ""
    other = ""

    # There can be multiple lines, so split by newline
    lines = text.split('\n')

    startedNonComments = False

    for line in lines:
        if not line.startswith('//'):
            startedNonComments = True
        
        if startedNonComments:
            other = other + line + '\n'    
        else:
            comments = comments + line + '\n'

    return (comments, other)
     
def wipeComments(text):
    text = re.sub(r'//.*?\n', '', text) # to end of line
    text = re.sub(r'//.*$', '', text) # to end of string
    return text

def hasDeclaration(text):
    """
       Consider a list of input arguments. Wipe comments. Wipe all after the equal
       sign. Split by commas. If each produced entity has a space or tab, it is a 
       declaration. So: myfun(x) has no declarations. But myfun(double x) has a
       declaration.
    """
    # Wipe comments    
    text = wipeComments(text)
    
    # Wipe until any starting parenthesis
    text = re.sub(r'^.*?\(', '', text)

    # Wipe last parenthesis and everything after it
    text = re.sub(r'\).*$', '', text)
    
    # Split by commas
    args = text.split(',')
    
    for arg in args:
        arg = arg.strip()
        # Wipe everything after the equal sign
        arg = re.sub(r'=.*$', '', arg)
        
        if not ' ' in arg and not '\t' in arg:
            return False
            
    return True
    
def parseFunction(text):
    """
    Find the function name in the text. It is the word before the opening parenthesis.
    Return an empty string if not found.
    """
    
    # Wipe any comments so it does not interfere with the match
    (comments, text) = splitByTopComments(text)
    
    # Match across multiple lines
    prefix = ""
    funName = ""
    (prefix, funName, args, other) = ('', '', '', '')
    m = re.match(r'^(.*?)(\w+)\s*(\(.*?)$', text, re.DOTALL)
    if not m:
        return (prefix, funName, args, other)
        
    prefix = m.group(1)
    funName = m.group(2)
    other = m.group(3)
    
    prefix = comments + prefix
    (args, other) = findBalancedBlock(other)
    
    hasDecl = hasDeclaration(args)
    if not hasDecl:
        return ('', '', '', '')
     
    return (prefix, funName, args, other)
  
def parseFunctions(text):
    """
    Parse the text and find all the functions in it.
    Return a dictionary from function name to arguments.
    """
    
    # Split the text by empty lines
    blocks = splitByEmptyLines(text)

    # Have a dictionary from function name to function body
    # For now every function name must be unique
    functions = {}
    for block in blocks:
        (prefix, fname, args, other) = parseFunction(block)
        if fname == '':
            continue

        if not fname in functions:
            functions[fname] = []
        functions[fname] += [args]
    
    return functions

def numCommas(s):
    """
        Return the number of commas in a string.
    """
    return s.count(',')

def replaceWithMostSimilar(s, arr):
    """
        Replace the string s with the one from the array arr that has the most
        similar number of commas. This is not fully robust.
    """
    num = len(arr)
    if num == 0:
        # Nothing to compare to
        return s

    # Find the number of commas in s
    numCommasS = numCommas(s)

    # Find the entry in arr that has the most similar number of commas
    best = arr[0]
    bestDiff = abs(numCommasS - numCommas(best))
    for i in range(1, num):
        diff = abs(numCommasS - numCommas(arr[i]))
        if diff < bestDiff:
            best = arr[i]
            bestDiff = diff

    return best
        
def replaceArgs(text, functions):
    """
    Parse the text in the cc file and find all the functions in it.
    Replace the function arguments with what is in the input.
    That came from the header file.
    """
    
    # Split the text by empty lines
    blocks = splitByEmptyLines(text)
    
    # Have a dictionary from function name to function body
    # For now every function name must be unique
    for i in range(len(blocks)):
    
        block = blocks[i]
        (prefix, fname, args, other) = parseFunction(block)
        if fname == '':
            continue

        if fname not in functions:
            continue
        
        # Replace with the most similar match from the header file
        args = replaceWithMostSimilar(args, functions[fname])
        
        # Remove default
        args = rmDefaultInArgStr(args)
        
        blocks[i] = prefix + fname + args + other
    
    text = '\n\n'.join(blocks)

    return text
    
def readText(filename):
    """
    Read the text from a file.
    """
    text = ''
    with open(filename, 'r') as f:
        text = f.read()
    return text

def updateCcBasedOnHeader(headerFile, ccFile):
    """
    Update the cc file based on the header file.
    """

    # Check that the header file ends in .h
    if not headerFile.endswith('.h'):
        raise Exception("Expecting a header file with extension .h")
        
    # Parse the functions in the header file
    text = readText(headerFile)
    functions = parseFunctions(text)
    
    # Replace the arguments with the ones from the header file
    text = readText(ccFile)
    text = replaceArgs(text, functions)

    # Save the text to the cc file
    print("Updating: " + ccFile)
    with open(ccFile, 'w') as f:
        f.write(text)
    
# Main code
if __name__ == '__main__':

    # Ensure there is at least one arg
    if len(sys.argv) < 2:
        print("Usage: update_cpp.py header.h")
        sys.exit(1)
        
    # Read the header file
    headerFile = sys.argv[1]

    # Let the cc file end in .cc instead of .h
    ccPref = headerFile.replace('.h', '')
    
    # Do a glob over *cc as sometimes there can be several cc files
    ccFiles = glob.glob(ccPref + '*.cc')

    for ccFile in ccFiles:
        updateCcBasedOnHeader(headerFile, ccFile)
        
    

