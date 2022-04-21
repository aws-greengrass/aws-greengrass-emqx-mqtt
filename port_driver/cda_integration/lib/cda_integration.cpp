#include <iostream>
#include "cda_integration.h"

class ClientDeviceAuthIntegration {
public:
    ClientDeviceAuthIntegration();
    void close() const;
    bool on_client_authenticate(const char*);
private:
};

ClientDeviceAuthIntegration::ClientDeviceAuthIntegration() {
}

void ClientDeviceAuthIntegration::close() const {
}

bool ClientDeviceAuthIntegration::on_client_authenticate(const char* clientId) {
    std::cout << "on_client_authenticate called with clientId: " << clientId << std::endl;
    return true;
}

CDA_INTEGRATION_HANDLE* cda_integration_init() {
    ClientDeviceAuthIntegration* cda_integ = nullptr;

    try {
        cda_integ = new ClientDeviceAuthIntegration();
    }
    catch( std::exception& e ) {
        std::cerr << e.what() << std::endl;
    }
    catch(...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return reinterpret_cast<CDA_INTEGRATION_HANDLE*>(cda_integ);
}

bool cda_integration_close(CDA_INTEGRATION_HANDLE* handle) {
    if(!handle) {
        std::cerr << "Handle cannot be null" << std::endl;
        return false;
    }

    ClientDeviceAuthIntegration* cda_integ = reinterpret_cast<ClientDeviceAuthIntegration*>(handle);
    try {
        cda_integ->close();
    }
    catch( std::exception& e ) {
        std::cerr << e.what() << std::endl;
        return false;
    }
    catch(...) {
        std::cerr << "Unknown exception" << std::endl;
        return false;
    }

    delete cda_integ;
    return true;
}

bool on_client_authenticate(CDA_INTEGRATION_HANDLE* handle, const char* clientId) {
    if(!handle) {
        std::cerr << "Handle cannot be null" << std::endl;
        return false;
    }

    ClientDeviceAuthIntegration* cda_integ = reinterpret_cast<ClientDeviceAuthIntegration*>(handle);
    try {
        return cda_integ->on_client_authenticate(clientId);
    }
    catch( std::exception& e ) {
        std::cerr << e.what() << std::endl;
        return false;
    }
    catch(...) {
        std::cerr << "Unknown exception" << std::endl;
        return false;
    }
}