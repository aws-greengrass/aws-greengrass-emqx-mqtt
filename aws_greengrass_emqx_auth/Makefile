## shallow clone for speed

BUILD_WITHOUT_QUIC ?= true
export BUILD_WITHOUT_QUIC

REBAR = rebar3

.PHONY: all
all: compile

.PHONY: compile
compile:
	$(REBAR) compile

.PHONY: ct
ct:
	$(REBAR) as test ct -v

.PHONY: eunit
eunit:
	$(REBAR) as test eunit

.PHONY: xref
xref:
	$(REBAR) xref

.PHONY: cover
cover:
	$(REBAR) cover

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock

.PHONY: release
release:
	$(REBAR) release
