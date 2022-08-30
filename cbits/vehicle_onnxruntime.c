// Copied libonnxruntime.so* to usr/lib/
#include <stdio.h>
#include <assert.h>

#include "vehicle_onnxruntime.h"
#include "onnxruntime/core/session/onnxruntime_c_api.h"

#ifdef _WIN32
#define tcscmp wcscmp
#else
#define tcscmp strcmp
#endif

const OrtApi* g_ort = NULL;

#define ORT_ABORT_ON_ERROR(expr)                             \
  do {                                                       \
    OrtStatus* onnx_status = (expr);                         \
    if (onnx_status != NULL) {                               \
      const char* msg = g_ort->GetErrorMessage(onnx_status); \
      fprintf(stderr, "%s\n", msg);                          \
      g_ort->ReleaseStatus(onnx_status);                     \
      abort();                                               \
    }                                                        \
  } while (0);


int onnxruntimeHS_Init()
{
  g_ort = OrtGetApiBase()->GetApi(ORT_API_VERSION);
  if (!g_ort) {
    fprintf(stderr, "Failed to init ONNX Runtime engine.\n");
    return -1;
  }
  return 0;
}

const char* onnxruntimeHS_GetVersionString()
{
  return OrtGetApiBase()->GetVersionString();
}

OrtEnv* onnxruntimeHS_CreateEnv()
{
  OrtEnv* env;
  ORT_ABORT_ON_ERROR(g_ort->CreateEnv(ORT_LOGGING_LEVEL_WARNING, "vehicle", &env));
  assert(env != NULL);
  return env;
}

void onnxruntimeHS_ReleaseEnv(OrtEnv* env)
{
  g_ort->ReleaseEnv(env);
}

OrtSessionOptions* onnxruntimeHS_CreateSessionOptions()
{
  OrtSessionOptions* session_options;
  ORT_ABORT_ON_ERROR(g_ort->CreateSessionOptions(&session_options));
  assert(session_options != NULL);
  return session_options;
}

void onnxruntimeHS_ReleaseSessionOptions(OrtSessionOptions* session_options)
{
  g_ort->ReleaseSessionOptions(session_options);
}

OrtSession* onnxruntimeHS_CreateSession(const OrtEnv* env, const OrtSessionOptions* session_options, const char* model_path)
{
  OrtSession* session;
  ORT_ABORT_ON_ERROR(g_ort->CreateSession(env, model_path, session_options, &session));
  assert(session != NULL);
  return session;
}

void onnxruntimeHS_ReleaseSession(OrtSession* session)
{
  g_ort->ReleaseSession(session);
}

void onnxruntimeHS_ReleaseTypeInfo(OrtTypeInfo* type_info)
{
  g_ort->ReleaseTypeInfo(type_info);
}

size_t onnxruntimeHS_SessionGetInputCount(const OrtSession* session)
{
  size_t size;
  ORT_ABORT_ON_ERROR(g_ort->SessionGetInputCount(session, &size));
  return size;
}

OrtTypeInfo* onnxruntimeHS_SessionGetInputTypeInfo(const OrtSession* session, size_t index)
{
  OrtTypeInfo* type_info;
  ORT_ABORT_ON_ERROR(g_ort->SessionGetInputTypeInfo(session, index, &type_info));
  assert(type_info != NULL);
  return type_info;
}

const OrtTensorTypeAndShapeInfo* onnxruntimeHS_CastTypeInfoToTensorInfo(const OrtTypeInfo* type_info)
{
  const OrtTensorTypeAndShapeInfo* tensor_info;
  ORT_ABORT_ON_ERROR(g_ort->CastTypeInfoToTensorInfo(type_info, &tensor_info));
  assert(tensor_info != NULL);
  return tensor_info;
}