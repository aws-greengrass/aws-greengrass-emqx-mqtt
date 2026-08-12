# Greengrass EMQX Auth Plugin Design

## Overview

The GG auth plugin (`gg.erl`) hooks into EMQX's client lifecycle to provide
Greengrass-managed authentication and authorization via the Client Device Auth
(CDA) component.

## Hooks

The plugin registers three hooks at `HP_HIGHEST` priority:

- `client.connect` — stores the peer certificate and MQTT protocol version in the process dictionary for use in subsequent hooks.
- `client.authenticate` — calls the CDA port driver to obtain an auth token for the connecting client. On success, stops the hook chain. On failure, behavior depends on `authMode`.
- `client.authorize` — calls the CDA port driver to check if the client is authorized for the requested pub/sub operation. Uses the auth token obtained during authentication.

## Auth Modes

Configured via `authMode` in the component configuration:

- `enabled` (default) — GG auth is authoritative. Auth failures reject the client.
- `bypass_on_failure` — GG auth is attempted first. On failure, continues the hook chain (defers to remaining auth providers).
- `bypass` — GG auth is disabled entirely. EMQX's own auth chain handles all clients.

## Per-Listener Behavior

The plugin respects the EMQX per-listener `enable_authn` setting. EMQX places this
setting in the `ClientInfo` map that is passed to hook callbacks.

When `enable_authn` is `false`:
- **Authentication**: the plugin skips GG auth processing. EMQX also natively skips
  the authenticate hook chain for these listeners, so the plugin's check is defense-in-depth.
- **Authorization**: the plugin skips GG authZ. GG authZ requires an auth token from
  GG authn, which cannot exist for clients that bypassed authentication. Authorization
  falls through to the EMQX authorization chain (`authorization.sources` + `authorization.no_match`).

## Port Driver

The plugin communicates with the Greengrass nucleus via a native port driver
(`gg_port_driver.erl` → `port_driver.cpp`). The port driver uses IPC to call
the Client Device Auth component for:

- `get_auth_token` — authenticate a client by client ID and certificate PEM
- `on_client_check_acl` — authorize a pub/sub action given a client ID, auth token, topic, and action

## Certificate Management

`gg_certs.erl` requests the CDA component to generate server TLS certificates for
the EMQX SSL listener. Certificates are written to the EMQX component work directory
where the SSL listener is configured to read them from.

`gg_conf.erl` sets a custom TLS verify function on the SSL listener via
`gg_listeners:put_verify_fun/3` (`gg_tls:verify_client_certificate/3`) to validate
client device certificates through Greengrass.

## Configuration

`gg_conf.erl` manages plugin configuration. It subscribes to configuration updates
from the Greengrass nucleus via IPC and applies them to both the plugin settings
(`authMode`) and the EMQX override configuration (`emqxConfig`).
