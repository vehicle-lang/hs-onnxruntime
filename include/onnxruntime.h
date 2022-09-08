#ifndef __onnxruntime_H__
#define __onnxruntime_H__

#include "onnxruntime/core/session/onnxruntime_c_api.h"

int onnxruntimeHS_Init();
const char* onnxruntimeHS_GetVersionString();

OrtEnv* onnxruntimeHS_CreateEnv();
void onnxruntimeHS_ReleaseEnv(OrtEnv* env);

OrtSessionOptions* onnxruntimeHS_CreateSessionOptions();
void onnxruntimeHS_ReleaseSessionOptions(OrtSessionOptions* session_options);

OrtSession* onnxruntimeHS_CreateSession(const OrtEnv* env, const OrtSessionOptions* session_options, const char* model_path);
void onnxruntimeHS_ReleaseSession(OrtSession* session);

void onnxruntimeHS_ReleaseTypeInfo(OrtTypeInfo* type_info);
const OrtTensorTypeAndShapeInfo* onnxruntimeHS_CastTypeInfoToTensorInfo(const OrtTypeInfo* type_info);

size_t onnxruntimeHS_SessionGetInputCount(const OrtSession* session);
OrtTypeInfo* onnxruntimeHS_SessionGetInputTypeInfo(const OrtSession* session, size_t index);

#endif /* __onnxruntime_H__ */