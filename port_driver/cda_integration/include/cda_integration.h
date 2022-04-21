#ifndef _CDA_INTEGRATION_H_
#define _CDA_INTEGRATION_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * @brief Handle to client device auth integration
 */
typedef void* CDA_INTEGRATION_HANDLE;

CDA_INTEGRATION_HANDLE* cda_integration_init();

bool on_client_authenticate(CDA_INTEGRATION_HANDLE* handle, const char* clientId);

bool cda_integration_close(CDA_INTEGRATION_HANDLE* handle);

#ifdef __cplusplus
}
#endif

#endif /* #ifndef _CDA_INTEGRATION_H_ */