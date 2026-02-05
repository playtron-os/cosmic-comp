export prefix ?= /usr
sysconfdir ?= /etc
bindir = $(prefix)/bin
libdir = $(prefix)/lib
sharedir = $(prefix)/share

BINARY = cosmic-comp
CARGO_TARGET_DIR ?= target
TARGET = debug
DEBUG ?= 0

.PHONY = all clean install uninstall vendor

ifeq ($(DEBUG),0)
	TARGET = release
	ARGS += --release
endif

VENDOR ?= 0
ifneq ($(VENDOR),0)
	ARGS += --offline --locked
endif

TARGET_BIN="$(DESTDIR)$(bindir)/$(BINARY)"

KEYBINDINGS_CONF="$(DESTDIR)$(sharedir)/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults"
TILING_EXCEPTIONS_CONF="$(DESTDIR)$(sharedir)/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults"
VOICE_MODE_DIR="$(DESTDIR)$(sharedir)/cosmic/com.playtron.VoiceMode/v1/defaults"

all: extract-vendor
	cargo build $(ARGS)

clean:
	cargo clean

distclean:
	rm -rf .cargo vendor vendor.tar target

vendor:
	mkdir -p .cargo
	cargo vendor | head -n -1 > .cargo/config
	echo 'directory = "vendor"' >> .cargo/config
	tar pcf vendor.tar vendor
	rm -rf vendor

extract-vendor:
ifeq ($(VENDOR),1)
	rm -rf vendor; tar pxf vendor.tar
endif

install:
	install -Dm0755 "$(CARGO_TARGET_DIR)/$(TARGET)/$(BINARY)" "$(TARGET_BIN)"
	install -Dm0644 "data/keybindings.ron" "$(KEYBINDINGS_CONF)"
	install -Dm0644 "data/tiling-exceptions.ron" "$(TILING_EXCEPTIONS_CONF)"

install-voice-mode:
	mkdir -p "$(VOICE_MODE_DIR)"
	install -m0644 data/voice-mode/* "$(VOICE_MODE_DIR)/"

uninstall-voice-mode:
	rm -rf "$(VOICE_MODE_DIR)"

# Local user config targets (for development)
LOCAL_VOICE_MODE_DIR = $(HOME)/.config/cosmic/com.playtron.VoiceMode/v1

install-voice-mode-local:
	mkdir -p "$(LOCAL_VOICE_MODE_DIR)"
	install -m0644 data/voice-mode/* "$(LOCAL_VOICE_MODE_DIR)/"

uninstall-voice-mode-local:
	rm -rf "$(LOCAL_VOICE_MODE_DIR)"

install-bare-session: install
	install -Dm0644 "data/cosmic.desktop" "$(DESTDIR)$(sharedir)/wayland-sessions/cosmic.desktop"
	install -Dm0644 "data/cosmic-session.target" "$(DESTDIR)$(libdir)/systemd/user/cosmic-session.target"
	install -Dm0644 "data/cosmic-session-pre.target" "$(DESTDIR)$(libdir)/systemd/user/cosmic-session-pre.target"
	install -Dm0644 "data/cosmic-comp.service" "$(DESTDIR)$(libdir)/systemd/user/cosmic-comp.service"
	install -Dm0755 "data/cosmic-service" "$(DESTDIR)/$(bindir)/cosmic-service"

uninstall:
	rm "$(TARGET_BIN)" "$(KEYBINDINGS_CONF)"
	rm -rf "$(VOICE_MODE_DIR)"

uninstall-bare-session:
	rm "$(DESTDIR)$(sharedir)/wayland-sessions/cosmic.desktop"

# RPM packaging
VERSION = $(shell grep '^version' Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/')
RPM_ROOT = $(CARGO_TARGET_DIR)/rpm-root
RPM_BUILD = $(CARGO_TARGET_DIR)/rpm-build

rpm: all
	@echo "Building RPM for $(BINARY) version $(VERSION)..."
	rm -rf "$(RPM_ROOT)" "$(RPM_BUILD)"
	mkdir -p "$(RPM_ROOT)" "$(RPM_BUILD)"/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
	COSMIC_COMP_SOURCE="$(CURDIR)" COSMIC_COMP_VERSION="$(VERSION)" rpmbuild -bb --nodeps \
		--define "_topdir $(CURDIR)/$(RPM_BUILD)" \
		--define "_binary_payload w2.xzdio" \
		--buildroot "$(CURDIR)/$(RPM_ROOT)" \
		packaging/rpm/cosmic-comp.spec
	mkdir -p dist
	cp -v $(RPM_BUILD)/RPMS/*/*.rpm dist/
	@echo "RPM copied to dist/"

run-debug:
	WAYLAND_DISPLAY=wayland-2 \
	COSMIC_COMP_LOG=warn,cosmic_comp::shell::layout::floating=debug,cosmic_comp::backend::render=debug,cosmic_comp::wayland::handlers::surface_embed=info,cosmic_comp::backend::kms=debug,cosmic_comp::shell=debug,cosmic_comp::shell::workspace=debug,cosmic_comp::backend::kms::surface=debug \
	stdbuf -oL cargo run --features debug 2>&1 \
	| stdbuf -oL sed -r 's/\x1B\[[0-9;]*[A-Za-z]//g' \
	| stdbuf -oL grep -v 'smithay::backend::renderer::gles' \
	| tee test.log
