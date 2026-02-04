Name:           cosmic-comp
Epoch:          1
Version: 1.0.1
Release:        1%{?dist}
Summary:        COSMIC Wayland Compositor (Playtron fork)

License:        GPL-3.0-only
URL:            https://github.com/pop-os/cosmic-comp
Source0:        %{name}-%{_arch}.tar.gz

# Runtime dependencies (from upstream cosmic-comp)
# Pin cosmic-icon-theme to 1.0.x series
Requires:       (cosmic-icon-theme >= 1.0.0 with cosmic-icon-theme < 1.1.0)
Requires:       mesa-libEGL
Requires:       libwayland-server
Requires:       libinput
Requires:       libseat
Requires:       libxkbcommon
Requires:       mesa-libgbm
Requires:       libdisplay-info
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

%files
%license %{_datadir}/licenses/cosmic-comp/LICENSE
%{_bindir}/cosmic-comp
%{_datadir}/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults
%{_datadir}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/primary_binding
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/fallback_binding
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/chat_app_id
%{_datadir}/cosmic/com.playtron.VoiceMode/v1/enabled

%changelog
* Thu Jan 09 2026 Playtron <dev@playtron.one> - 1.0.0-1
- Initial RPM package
