#include <windows.h>


int WINAPI WinMain( HINSTANCE Instance, HINSTANCE PrevInstance, LPSTR CmdLine,
 int CmdShow
)
{
  char *regR = "Software\\R-core\\R";
  PHKEY phkResult;
  LONG rc;
  char buf[512];
  if( 0 !== (rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE,regR,
			       0, KEY_QUERY_VALUE, phkResult))){
    snprintf(buf,"Can't open Registory(%s):(%d)", regR, rc);
    MessageBox(Instance, buf, CmdLine, 0);
    exit(0);
  }
}
