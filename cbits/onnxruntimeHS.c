/* -----------------------------------------------------------------------------
 * (c) Wen Kokke 2021
 * ----------------------------------------------------------------------------- */

#include "onnxruntimeHS.h"

const char* OrtApiBase_GetVersionString(OrtApiBase *ortApiBase) {
  return ortApiBase->GetVersionString();
}