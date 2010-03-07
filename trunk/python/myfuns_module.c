#include <Python.h>

// Create a C module that will be called from Python. See the
// code myfuns_build.py about how to compile and run this module.

// The function doing the work. It takes an input Python string,
// converts it to a C-style string, and calls the 'system' command
// with that string. It takes the output of that command (an integer)
// and converts it to a Python integer which is returned from
// this function.
static PyObject * myfun1(PyObject *self, PyObject *args){
  
  const char *command;
  const int v;
  int sts;
  
  if (!PyArg_ParseTuple(args, "si", &command, &v))
    return NULL; // NULL will be interpreted as an error flag

  printf("The user input was: %s %d\n", command, v);
  sts = system(command);
  sts += v;
  return Py_BuildValue("i", sts);

}

// List all the functions implemented in this module.
static PyMethodDef allMyFuns[] = {
  {"myfun1",  myfun1, METH_VARARGS,
   "A simple C function which just executes a shell command."},
  {NULL, NULL, 0, NULL}        /* Sentinel, must be last */
};


// Initialization routine.
PyMODINIT_FUNC initmyfuns(void){
  (void) Py_InitModule("myfuns", allMyFuns);
}

int main(int argc, char *argv[])
{
  /* Pass argv[0] to the Python interpreter */
  Py_SetProgramName(argv[0]);
  
  /* Initialize the Python interpreter.  Required. */
  Py_Initialize();
  
  /* Add a static module */
  initmyfuns();

  return 0;
}
