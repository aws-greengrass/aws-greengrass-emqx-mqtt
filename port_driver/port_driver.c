/* port_driver.c */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <erl_driver.h>
#include <ei.h>
#include <cda_integration.h>

// OPERATIONS
#define ON_CLIENT_AUTHENTICATE 1

// RETURN CODES
#define RETURN_CODE_SUCCESS    0
#define RETURN_CODE_UNKNOWN_OP 1

// TODO: Improve logging. Add timestamp to the logs
#define LOG(str,args...) printf("%s:%d %s() "str"\n",__FILE__,__LINE__,__func__, ##args)

typedef struct {
    ErlDrvPort port;
    CDA_INTEGRATION_HANDLE* cda_integration_handle;
} DriverContext;

static ErlDrvData drv_start(ErlDrvPort port, char *buff)
{
    LOG("Starting the driver");
    ei_init();
    DriverContext* context = (DriverContext*)driver_alloc(sizeof(DriverContext));
    context->port = port;
    context->cda_integration_handle = cda_integration_init();
    return (ErlDrvData)context;
}

static void drv_stop(ErlDrvData handle)
{
    LOG("Stopping the driver");
    DriverContext* context = (DriverContext*)handle;
    cda_integration_close(context->cda_integration_handle);
    driver_free((char*)handle);
}

static unsigned int get_operation(ErlIOVec *ev) {
    ErlDrvBinary* bin = ev->binv[1];
    return bin->orig_bytes[0];
}

static void write_bool_to_port(DriverContext* context, bool result, char return_code) {
    ErlDrvBinary* out = driver_alloc_binary(sizeof(result));
    memcpy(&out->orig_bytes[0], &result, sizeof(result));
    driver_output_binary(context->port, &return_code, 1, out, 0, sizeof(result));
    driver_free_binary(out);
}

static void handle_on_client_authenticate(DriverContext* context, ErlIOVec *ev) {
    ErlDrvBinary* bin = ev->binv[1];
    char* buf = &bin->orig_bytes[1];

    int index = 0;
    int version = 0;
    ei_decode_version(buf, &index, &version);

    int type;
    int entry_size;
    int res = ei_get_type(buf, &index, &type, &entry_size);

    char *decoded_value = (char *)malloc(sizeof(char) * (entry_size + 1));
    ei_decode_string(buf, &index, decoded_value);
    bool result = on_client_authenticate(context->cda_integration_handle, decoded_value);
    free(decoded_value);

    char return_code = RETURN_CODE_SUCCESS;
    write_bool_to_port(context, result, return_code);
}

static void handle_unknown_op(DriverContext* context) {
    bool result = false;
    char return_code = RETURN_CODE_UNKNOWN_OP;
    write_bool_to_port(context, result, return_code);
}

static void drv_output(ErlDrvData handle, ErlIOVec *ev)
{
    DriverContext* context = (DriverContext*)handle;
    const unsigned int op = get_operation(ev);
    switch(op) {
        case ON_CLIENT_AUTHENTICATE:
            handle_on_client_authenticate(context, ev);
            break;
        default:
            LOG("Unknown operation: %u", op);
            handle_unknown_op(context);
    }
}

ErlDrvEntry driver_entry = {
    NULL,			/* F_PTR init, called when driver is loaded */
    drv_start,		/* L_PTR start, called when port is opened */
    drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,   		/* F_PTR output, called when erlang has sent */
    NULL,			/* F_PTR ready_input, called when input descriptor ready */
    NULL,			/* F_PTR ready_output, called when output descriptor ready */
    "port_driver",	/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,                       /* void *handle, Reserved by VM */
    NULL,			/* F_PTR control, port_command callback */
    NULL,			/* F_PTR timeout, reserved */
    drv_output,		/* F_PTR outputv, reserved */
    NULL,                       /* F_PTR ready_async, only for async drivers */
    NULL,                       /* F_PTR flush, called when port is about
				   to be closed, but there is data in driver
				   queue */
    NULL,                       /* F_PTR call, much like control, sync call
				   to driver */
    NULL,                       /* unused */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be
				   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be
				       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be
				       set to this value */
    0,                          /* int driver_flags, see documentation */
    NULL,                       /* void *handle2, reserved for VM use */
    NULL,                       /* F_PTR process_exit, called when a
				   monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an
				   event object */
};

DRIVER_INIT(port_driver) /* must match name in driver_entry */
{
    return &driver_entry;
}
