#!/bin/bash

trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

. $DOTPATH/etc/lib/vital.sh

is_osx || die "osx only"

# Dock {{{1
set_dock_preferences()
{
    # Set the icon size
    defaults write com.apple.dock tilesize -int 20

    # Magnificate the Dock
    defaults write com.apple.dock magnification -bool true

    # Set the Magnification size
    defaults write com.apple.dock largesize -int 120

    # Reduce Dock clutter by minimizing windows into their application icons
    defaults write com.apple.dock minimize-to-application -bool true

    # Automatically hide or show the Dock
    defaults write com.apple.dock autohide -bool true

    # Enable spring loading for all Dock items
    defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true

    # Set Hot Corners
    # Top left screen corner → Desktop
    defaults write com.apple.dock wvous-tl-corner -int 4
    defaults write com.apple.dock wvous-tl-modifier -int 0

    # Top right screen corner → Put display to sleep
    defaults write com.apple.dock wvous-tr-corner -int 10
    defaults write com.apple.dock wvous-tr-modifier -int 0

    # Bottom left screen corner → Mission Control
    defaults write com.apple.dock wvous-bl-corner -int 2
    defaults write com.apple.dock wvous-bl-modifier -int 0

    # Bottom right screen corner → Launchpad
    defaults write com.apple.dock wvous-br-corner -int 11
    defaults write com.apple.dock wvous-br-modifier -int 0
}

# QuickLook {{{1
set_quicklook_preferences()
{
    # Allow you to select and copy string in QuickLook
    defaults write com.apple.finder QLEnableTextSelection -bool true
}


# Finder {{{1
set_finder_preferences()
{
    # Set `${HOME}` as the default location for new Finder windows
    defaults write com.apple.finder NewWindowTarget -string "PfHm"
    defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"

    # Hide icons for hard drives, servers, and removable media on the desktop
    defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false
    defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
    defaults write com.apple.finder ShowMountedServersOnDesktop -bool false
    defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false

    # Finder: show all filename extensions
    defaults write NSGlobalDomain AppleShowAllExtensions -bool true

    # Finder: show status bar
    defaults write com.apple.finder ShowStatusBar -bool true

    # Finder: show path bar
    defaults write com.apple.finder ShowPathbar -bool true

    # Display full POSIX path as Finder window title
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

    # Keep folders on top when sorting by name
    defaults write com.apple.finder _FXSortFoldersFirst -bool true

    # When performing a search, search the current folder by default
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

    # Disable the warning when changing a file extension
    defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

    # Disable the warning when removing from iCloud Drive
    defaults write com.apple.finder FXEnableRemoveFromICloudDriveWarning -bool false

    # Enable spring loading for directories
    defaults write NSGlobalDomain com.apple.springing.enabled -bool true

    # Avoid creating .DS_Store files on network volumes
    defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

    # Disable disk image verification
    defaults write com.apple.frameworks.diskimages skip-verify -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

    # Automatically open a new Finder window when a volume is mounted
    defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
    defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
    defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

    # Use colomn view in all Finder windows by default
    defaults write com.apple.finder FXPreferredViewStyle -string "ilsv"

    # Disable the warning before emptying the Trash
    defaults write com.apple.finder WarnOnEmptyTrash -bool false

    # Enable AirDrop over Ethernet and on unsupported Macs running Lion
    defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true

    # Show the ~/Library folder
    chflags nohidden ~/Library
}

# Keyboard {{{1
set_keyboard_preferences()
{
    # Disable press-and-hold for keys in favor of key repeat
    defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

    # Set a blazingly fast keyboard repeat rate
    defaults write NSGlobalDomain KeyRepeat -int 2
    defaults write NSGlobalDomain InitialKeyRepeat -int 25

    # Disable auto-correct
    defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

    # Disable Control-Command-D for Emacs<
    defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

}

# Trackpad {{{1
set_trackpad_preferences()
{
    # Enable tap to click for this user and for the login screen
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
    defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
    defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1


}

# Safari.app {{{1
set_safari_preferences()
{
    # Set Safari’s home page to Top Sites
    defaults write com.apple.Safari HomePage -string "topsites://"

    # Prevent Safari from opening ‘safe’ files automatically after downloading
    defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

    # Enable Safari’s debug menu
    defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

    # Enable the Develop menu and the Web Inspector in Safari
    defaults write com.apple.Safari IncludeDevelopMenu -bool true
    defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
    defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

    # Add a context menu item for showing the Web Inspector in web views
    defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

    # Disable auto-correct
    defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false
}

# Terminal.app {{{1
set_terminal_preferences()
{
    # Make the focus automatically follow the mouse
    defaults write com.apple.terminal FocusFollowsMouse -bool true

    # Only use UTF-8 in Terminal.app
    defaults write com.apple.terminal StringEncodings -array 4

    # Use a custom theme
    # Use a modified version of the Solarized Dark theme by default in Terminal.app
    TERM_PROFILE='~/Downloads/Solarized_Dark.terminal';
    CURRENT_PROFILE="$(defaults read com.apple.terminal 'Default Window Settings')";
    if [ "${CURRENT_PROFILE}" != "${TERM_PROFILE}" ]; then
        open "$TERM_PROFILE"
        defaults write com.apple.Terminal "Default Window Settings" -string "$TERM_PROFILE"
        defaults write com.apple.Terminal "Startup Window Settings" -string "$TERM_PROFILE"
    fi
    defaults import com.apple.Terminal "$HOME/Library/Preferences/com.apple.Terminal.plist"

    # Enable �gfocus follows mouse�h for Terminal.app and all X11 apps
    # i.e. hover over a window and start typing in it without clicking first
    #defaults write com.apple.terminal FocusFollowsMouse -bool true
    #defaults write org.x.X11 wm_ffm -bool true
}

# Spotlight {{{1
set_spotlight_preferences()
{
    # Hide Spotlight tray-icon (and subsequent helper)
    #sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search

    # Disable Spotlight indexing for any volume that gets mounted and has not yet
    # been indexed before.
    # Use `sudo mdutil -i off "/Volumes/foo"` to stop indexing any volume.
    sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"

    # Change indexing order and disable some search results
    # Yosemite-specific search results (remove them if your are using OS X 10.9 or older):
    #   MENU_DEFINITION
    #   MENU_CONVERSION
    #   MENU_EXPRESSION
    #   MENU_SPOTLIGHT_SUGGESTIONS (send search queries to Apple)
    #   MENU_WEBSEARCH             (send search queries to Apple)
    #   MENU_OTHER
    #defaults write com.apple.spotlight orderedItems -array \
    #    '{"enabled" = 1;"name" = "APPLICATIONS";}' \
    #    '{"enabled" = 1;"name" = "SYSTEM_PREFS";}' \
    #    '{"enabled" = 1;"name" = "DIRECTORIES";}' \
    #    '{"enabled" = 1;"name" = "PDF";}' \
    #    '{"enabled" = 1;"name" = "FONTS";}' \
    #    '{"enabled" = 0;"name" = "DOCUMENTS";}' \
    #    '{"enabled" = 0;"name" = "MESSAGES";}' \
    #    '{"enabled" = 0;"name" = "CONTACT";}' \
    #    '{"enabled" = 0;"name" = "EVENT_TODO";}' \
    #    '{"enabled" = 0;"name" = "IMAGES";}' \
    #    '{"enabled" = 0;"name" = "BOOKMARKS";}' \
    #    '{"enabled" = 0;"name" = "MUSIC";}' \
    #    '{"enabled" = 0;"name" = "MOVIES";}' \
    #    '{"enabled" = 0;"name" = "PRESENTATIONS";}' \
    #    '{"enabled" = 0;"name" = "SPREADSHEETS";}' \
    #    '{"enabled" = 0;"name" = "SOURCE";}' \
    #    '{"enabled" = 0;"name" = "MENU_DEFINITION";}' \
    #    '{"enabled" = 0;"name" = "MENU_OTHER";}' \
    #    '{"enabled" = 0;"name" = "MENU_CONVERSION";}' \
    #    '{"enabled" = 0;"name" = "MENU_EXPRESSION";}' \
    #    '{"enabled" = 0;"name" = "MENU_WEBSEARCH";}' \
    #    '{"enabled" = 0;"name" = "MENU_SPOTLIGHT_SUGGESTIONS";}'

    # Load new settings before rebuilding the index
    killall mds > /dev/null 2>&1

    # Make sure indexing is enabled for the main volume
    sudo mdutil -i on / > /dev/null

    # Rebuild the index from scratch
    sudo mdutil -E / > /dev/null
}

# UI/UX {{{1
set_ui_and_ux_preferences()
{
    # Disable Gatekeeper
    sudo spctl --master-disable

    # Hide the battery percentage from the menu bar
    #defaults write com.apple.menuextra.battery ShowPercent -string "NO"

    # Disable the "Are you sure you want to open this application?" dialog
    defaults write com.apple.LaunchServices LSQuarantine -bool false

    # Disable `Reopen windows when logging back in`
    #defaults write com.apple.loginwindow TALLogoutSavesState 0

    # Automatically quit the printer app once the print jobs are completed
    #defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

    # Enable subpixel font rendering on non-Apple LCDs
    defaults write NSGlobalDomain AppleFontSmoothing -int 2

    # Expand save panel by default
    defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true

    # Expand print panel by default
    defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true

    # Save to disk (not to iCloud) by default
    defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

    # Restart automatically if the computer freezes
    sudo systemsetup -setrestartfreeze on

}

# main {{{1}}}

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.osx` has finished
#while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &
while true
do
    sudo -n true
    sleep 60;
    kill -0 "$$" || exit
done 2>/dev/null &

#set_quicklook_preferences
set_dock_preferences
set_finder_preferences
set_keyboard_preferences
set_safari_preferences
set_terminal_preferences
set_trackpad_preferences
set_ui_and_ux_preferences

killall cfprefsd
killall Dock
killall Finder
killall Safari
killall SystemUIServer

# vim:fdm=marker expandtab fdc=3 ts=4 sw=4 sts=4:
