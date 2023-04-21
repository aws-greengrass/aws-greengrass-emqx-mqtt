## shallow clone for speed

BUILD_WITHOUT_QUIC ?= true
export BUILD_WITHOUT_QUIC

REBAR = $(CURDIR)/rebar3
REBAR_VERSION = 3.18.0-emqx-1

.PHONY: all
all: compile

.PHONY: get-rebar3
get-rebar3:
	@$(CURDIR)/get-rebar3 $(REBAR_VERSION)

$(REBAR):

.PHONY: compile
compile: $(REBAR)
	$(REBAR) compile

.PHONY: ct
ct: $(REBAR)
	$(REBAR) as test ct -v

.PHONY: eunit
eunit: $(REBAR)
	$(REBAR) as test eunit

.PHONY: xref
xref: $(REBAR)
	$(REBAR) xref

.PHONY: cover
cover: $(REBAR)
	$(REBAR) cover

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock

.PHONY: release
release: $(REBAR)
	$(REBAR) release
