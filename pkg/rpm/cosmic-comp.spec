Name:           cosmic-comp
Epoch:          1
Version: 1.28.6
Release:        1%{?dist}
Summary:        COSMIC Wayland Compositor (Playtron fork)

License:        GPL-3.0-only
URL:            https://github.com/pop-os/cosmic-comp
Source0:        %{name}.tar.gz

%global debug_package %{nil}

# Runtime dependencies (from upstream cosmic-comp)
# cosmic-icon-theme is noarch icon assets with no ABI coupling — bind to COSMIC 1.x
# (< 2.0.0), not a single minor, so a Fedora icon-theme bump can't downgrade this fork.
Requires:       (cosmic-icon-theme >= 1.0.0 with cosmic-icon-theme < 2.0.0)
Requires:       mesa-libEGL
Requires:       libwayland-server
Requires:       libinput
Requires:       libseat
Requires:       libxkbcommon
Requires:       mesa-libgbm
# libdisplay-info is statically linked into the binary (see Dockerfile); no runtime dep.
Requires:       pixman
Requires:       systemd-udev

# Override the upstream cosmic-comp from cosmic-desktop
Provides:       cosmic-comp = %{epoch}:%{version}-%{release}
Obsoletes:      cosmic-comp < %{epoch}:%{version}

%description
Wayland compositor for the COSMIC desktop environment.

%prep
%autosetup -n %{name} -p1

%build

%install
# COSMIC_COMP_SOURCE is set by the Makefile to the source directory
install -Dm0755 "usr/bin/cosmic-comp" "%{buildroot}%{_bindir}/cosmic-comp"
install -Dm0644 "usr/share/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults" "%{buildroot}%{_datadir}/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults"
install -Dm0644 "usr/share/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults" "%{buildroot}%{_datadir}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults"
install -Dm0644 "usr/share/licenses/cosmic-comp/LICENSE" "%{buildroot}%{_datadir}/licenses/cosmic-comp/LICENSE"

# Voice mode configuration (individual key files for cosmic-config)
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/primary_binding" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/primary_binding"
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/fallback_binding" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/fallback_binding"
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/chat_app_id" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/chat_app_id"
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/enabled" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/enabled"

# --- Persistent-compositor deployment kit (global-compositor login model).
# One persistent cosmic-comp hosts the greeter + desktop as wayland clients; these
# artifacts deploy that model.
install -Dm4755 "usr/libexec/agentos-session-launch"        "%{buildroot}%{_libexecdir}/agentos-session-launch"
install -Dm0755 "usr/libexec/agentos-greeter-launch"        "%{buildroot}%{_libexecdir}/agentos-greeter-launch"
install -Dm0755 "usr/libexec/agentos-session-logout"        "%{buildroot}%{_libexecdir}/agentos-session-logout"
install -Dm0644 "usr/lib/systemd/system/cosmic-comp-global.service"            "%{buildroot}%{_prefix}/lib/systemd/system/cosmic-comp-global.service"
install -Dm0644 "usr/lib/sysusers.d/agentos-compositor.conf"                    "%{buildroot}%{_prefix}/lib/sysusers.d/agentos-compositor.conf"
install -Dm0644 "usr/share/polkit-1/rules.d/50-agentos-desktop.rules"          "%{buildroot}%{_datadir}/polkit-1/rules.d/50-agentos-desktop.rules"
install -Dm0644 "usr/share/selinux/packages/agentos_greeter_compositor.te"     "%{buildroot}%{_datadir}/selinux/packages/agentos_greeter_compositor.te"
install -Dm0644 "usr/share/selinux/packages/agentos_greeter_compositor.fc"     "%{buildroot}%{_datadir}/selinux/packages/agentos_greeter_compositor.fc"

%post
# Persistent-compositor model: create the agentos-display + greeter users + compositor
# group, then compile+load the SELinux module that lets the greeter (xdm_t) reach the
# compositor's wayland socket in /run/cosmic-comp. Runs at install (image build), where
# systemd-sysusers + the SELinux build tools are present; guarded so a bare install is safe.
systemd-sysusers /usr/lib/sysusers.d/agentos-compositor.conf >/dev/null 2>&1 || :
if command -v checkmodule >/dev/null 2>&1 && command -v semodule >/dev/null 2>&1; then
    m=/usr/share/selinux/packages/agentos_greeter_compositor
    # Non-fatal (image build must not abort), but NOT silenced: a silent module/relabel failure
    # ships fake confinement (compositor keeps running unconfined_t while looking hardened).
    if checkmodule -M -m -o "$m.mod" "$m.te" && \
       semodule_package -o "$m.pp" -m "$m.mod" -f "$m.fc" && \
       semodule -i "$m.pp"; then
        # Apply cosmic_comp_exec_t so the binary triggers the transition into cosmic_comp_t
        # (the runtime dir is labeled by systemd's RuntimeDirectory at boot).
        restorecon -F /usr/bin/cosmic-comp || :
        semodule -l | grep -q agentos_greeter_compositor || \
            echo "WARNING: agentos_greeter_compositor SELinux module did not load" >&2
        matchpathcon /usr/bin/cosmic-comp 2>/dev/null | grep -q cosmic_comp_exec_t || \
            echo "WARNING: /usr/bin/cosmic-comp fcontext is not cosmic_comp_exec_t" >&2
    else
        echo "WARNING: failed to build/load agentos_greeter_compositor SELinux module" >&2
    fi
    rm -f "$m.mod" "$m.pp"
fi

%files
%license %{_datadir}/licenses/cosmic-comp/LICENSE
%{_bindir}/cosmic-comp
%{_datadir}/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults
%{_datadir}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/primary_binding
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/fallback_binding
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/chat_app_id
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/enabled
# Persistent-compositor deployment kit
%attr(4755,root,root) %{_libexecdir}/agentos-session-launch
%{_libexecdir}/agentos-greeter-launch
%{_libexecdir}/agentos-session-logout
%{_prefix}/lib/systemd/system/cosmic-comp-global.service
%{_prefix}/lib/sysusers.d/agentos-compositor.conf
%{_datadir}/polkit-1/rules.d/50-agentos-desktop.rules
%{_datadir}/selinux/packages/agentos_greeter_compositor.te
%{_datadir}/selinux/packages/agentos_greeter_compositor.fc

%changelog
* Thu Jan 09 2026 Playtron <dev@playtron.one> - 1.0.0-1
- Initial RPM package
