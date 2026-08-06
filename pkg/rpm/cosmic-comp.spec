Name:           cosmic-comp
Epoch:          1
Version: 1.34.2
Release:        1%{?dist}
Summary:        COSMIC Wayland Compositor (Playtron fork)

License:        GPL-3.0-only
URL:            https://github.com/pop-os/cosmic-comp
Source0:        %{name}.tar.gz

%global debug_package %{nil}

# SELinux module, compiled + loaded at %post (see scriptlets).
%global selinuxtype    targeted
%global selinuxmodule  agentos_greeter_compositor

# Runtime dependencies (from upstream cosmic-comp).
# cosmic-icon-theme: noarch, no ABI coupling — pin to COSMIC 1.x so a Fedora bump can't downgrade.
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

# SELinux: base policy for the module's required types; checkpolicy + policycoreutils compile
# and load the module at %post (compose).
Requires:         selinux-policy-%{selinuxtype}
Requires(post):   checkpolicy
Requires(post):   policycoreutils
Requires(postun): policycoreutils

# Override the upstream cosmic-comp from cosmic-desktop
Provides:       cosmic-comp = %{epoch}:%{version}-%{release}
Obsoletes:      cosmic-comp < %{epoch}:%{version}

%description
Wayland compositor for the COSMIC desktop environment.

%prep
%autosetup -n %{name} -p1

%build

%install
# Paths are relative to the unpacked Source0 tarball (staged usr/ tree; built by `go-task rpm`).
install -Dm0755 "usr/bin/cosmic-comp" "%{buildroot}%{_bindir}/cosmic-comp"
install -Dm0644 "usr/share/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults" "%{buildroot}%{_datadir}/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults"
install -Dm0644 "usr/share/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults" "%{buildroot}%{_datadir}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults"
install -Dm0644 "usr/share/licenses/cosmic-comp/LICENSE" "%{buildroot}%{_datadir}/licenses/cosmic-comp/LICENSE"

# Voice mode configuration (individual key files for cosmic-config)
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/primary_binding" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/primary_binding"
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/fallback_binding" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/fallback_binding"
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/chat_app_id" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/chat_app_id"
install -Dm0644 "usr/share/cosmic/com.playtron.VoiceMode/v1/enabled" "%{buildroot}%{_datadir}/cosmic/com.playtron.VoiceMode/v1/enabled"

# Persistent-compositor login kit: one cosmic-comp hosts the greeter + desktop as wayland clients.
install -Dm4755 "usr/libexec/agentos-session-launch"        "%{buildroot}%{_libexecdir}/agentos-session-launch"
install -Dm0755 "usr/libexec/agentos-greeter-launch"        "%{buildroot}%{_libexecdir}/agentos-greeter-launch"
install -Dm0755 "usr/libexec/agentos-session-logout"        "%{buildroot}%{_libexecdir}/agentos-session-logout"
install -Dm0755 "usr/libexec/agentos-x-cleanup"             "%{buildroot}%{_libexecdir}/agentos-x-cleanup"
install -Dm0644 "usr/lib/systemd/system/cosmic-comp-global.service"            "%{buildroot}%{_prefix}/lib/systemd/system/cosmic-comp-global.service"
install -Dm0644 "usr/lib/sysusers.d/agentos-compositor.conf"                    "%{buildroot}%{_prefix}/lib/sysusers.d/agentos-compositor.conf"
install -Dm0644 "usr/share/selinux/packages/agentos_greeter_compositor.te"     "%{buildroot}%{_datadir}/selinux/packages/agentos_greeter_compositor.te"
install -Dm0644 "usr/share/selinux/packages/agentos_greeter_compositor.fc"     "%{buildroot}%{_datadir}/selinux/packages/agentos_greeter_compositor.fc"

%post
# Users/group come from the shipped sysusers.d file, created by systemd-sysusers.service at
# boot (first boot + every image update) — no scriptlet: a raw systemd-sysusers call writes
# /etc in the rpm-ostree compose sandbox and may not reach the target.
#
# Compile the SELinux module (greeter -> compositor socket) at compose — checkpolicy is pulled
# in via Requires(post) — then load it at priority 200 (admin overrides still win) and label the
# binary cosmic_comp_exec_t. Non-fatal but WARNs loudly; a silent skip would ship fake confinement.
m=%{_datadir}/selinux/packages/%{selinuxmodule}
if checkmodule -M -m -o "$m.mod" "$m.te" && semodule_package -o "$m.pp" -m "$m.mod" -f "$m.fc"; then
%selinux_modules_install -s %{selinuxtype} "$m.pp"
    restorecon -F %{_bindir}/cosmic-comp || :
    semodule -l | grep -q %{selinuxmodule} || echo "WARNING: %{selinuxmodule} not in the module store" >&2
    # Store presence is not enough: if the deployed policy binary is a NEWER format than local
    # libsepol can write, semodule rebuilds a lower version that load_policy never picks up, and
    # the domain silently never exists. Check the RUNNING policy (only meaningful when active).
    if selinuxenabled 2>/dev/null; then
        runcon -t cosmic_comp_t /bin/true 2>/dev/null || \
            echo "WARNING: cosmic_comp_t absent from the running policy; compositor stays unconfined" >&2
    fi
else
    echo "WARNING: failed to build %{selinuxmodule} SELinux module" >&2
fi
rm -f "$m.mod" "$m.pp"

%postun
# Remove the module on final uninstall (the macro guards on $1 -eq 0 internally).
%selinux_modules_uninstall -s %{selinuxtype} %{selinuxmodule}

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
%{_libexecdir}/agentos-x-cleanup
%{_prefix}/lib/systemd/system/cosmic-comp-global.service
%{_prefix}/lib/sysusers.d/agentos-compositor.conf
%{_datadir}/selinux/packages/agentos_greeter_compositor.te
%{_datadir}/selinux/packages/agentos_greeter_compositor.fc

%changelog
* Thu Jan 09 2026 Playtron <dev@playtron.one> - 1.0.0-1
- Initial RPM package
