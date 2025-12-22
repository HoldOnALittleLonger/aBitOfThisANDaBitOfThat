#!/bin/bash

##
# wifi_connect - a bash script used to handle wifi connection based on wpa utilities
# commands:
#                up
#                down
#                scan
#                scan_results
#                connect <SSID>
#                disconnect
#                quick_connect <SSID>
#                status
#                reset
#                help
##

# wireless interface name
wifi_if=""

# this file is used to store scan results
wifi_scan_results=/tmp/802.11-scan-results

# directory in which wlan config is placed
wpa_config_dir=/etc/wpa_supplicant/wlans

wifi_LRU=$wpa_config_dir/last_recently_used

# path of wpa config,this file is created when the first time
# wpa utility is installed
wpa_original_config=/etc/wpa_supplicant/wpa_supplicant.conf

current_psk=""

WIFICONN_SSID_NOPSK=64

WIFICONN_EC_UP_IF=1
WIFICONN_EC_DOWN_IF=2

WIFICONN_EC_WPA_DAEMON=16
WIFICONN_EC_WPA_SCAN=17

WIFICONN_EC_WPA_NOCONF=18
WIFICONN_EC_WPA_NOPASS=19

WIFICONN_EC_WPA_ADDNET=20
WIFICONN_EC_WPA_SETSSID=21
WIFICONN_EC_WPA_SETPSK=22
WIFICONN_EC_WPA_SETKEYMGMT=23
WIFICONN_EC_WPA_ONNET=24
WIFICONN_EC_WPA_OFFNET=25
WIFICONN_EC_WPA_GETID=26

WIFICONN_EC_BADCMD=32

WIFICONN_EC_NORMAL=127

##
# perror - print error message
# $1:      error code
# $2:      additional msg
# return:  always 0
##
function perror()
{
    errmsg_head="wifi_connect: error:"
    errmsg="Unknown error"
    if [[ $# -eq 1 || $# -eq 2 ]]
    then
        ec=$1
        case $ec in
            $WIFICONN_EC_UP_IF)
                errmsg="Failed to set interface up"
                ;;
            $WIFICONN_EC_DOWN_IF)
                errmsg="Failed to set interface down"
                ;;
            $WIFICONN_EC_WAP_DAEMON)
                errmsg="Failed to start wpa_supplicant daemon"
                ;;
            $WIFICONN_EC_WPA_SCAN)
                errmsg="Failed to scan wlans"
                ;;
            $WIFICONN_EC_WPA_NOCONF)
                errmsg="No config file is found"
                ;;
            $WIFICONN_EC_WPA_NOPASS)
                errmsg="No passphrase is found"
                ;;
            $WIFICONN_EC_WPA_ADDNET)
                errmsg="Failed to add network"
                ;;
            $WIFICONN_EC_WPA_SETSSID)
                errmsg="Failed to set ssid"
                ;;
            $WIFICONN_EC_WPA_SETPSK)
                errmsg="Failed to set psk"
                ;;
            $WIFICONN_EC_WPA_SETKEYMGMT)
                errmsg="Failed to set key_mgmt"
                ;;
            $WIFICONN_EC_WPA_ONNET)
                errmsg="Failed to enable network"
                ;;
            $WIFICONN_EC_WPA_OFFNET)
                errmsg="Failed to disable network"
                ;;
            $WIFICONN_EC_WPA_GETID)
                errmsg="Failed to get network id"
                ;;
            $WIFICONN_EC_BADCMD)
                errmsg="Bad usage"
                ;;
        esac
    else
        errmsg="perror(): Bad calling"
    fi

    if [ $# -eq 2 ]
    then
        printf "%s %s - %s\n" "$errmsg_head" "$errmsg" "$2" > /dev/stderr
    else
        printf "%s %s\n" "$errmsg_head" "$errmsg" > /dev/stderr
    fi

    return 0
}

##
# start_wpa_supplicant - start wpa_supplicant daemon
# return:                0 => succeed OR $WIFICONN_EC_NORMAL => failed
# @ if the daemon is running,this function would not touch it
##
function start_wpa_supplicant()
{
    wpas_pid=$(pgrep wpa_supplicant)
    if [ $? -ne 0 ]
    then
        wpa_supplicant -i $wifi_if -B -c $wpa_original_config
        if [ $? -ne 0 ]
        then
            perror $WIFICONN_EC_WPA_DAEMON
            return $WIFICONN_EC_NORMAL
        fi
    fi

    return 0
}


##
# stop_wpa_supplicant - stop wpa_supplicant regardless to whether it is
#                       running
# return:               always 0
##
function stop_wpa_supplicant()
{
    wpa_cli -i $wifi_if terminate
    killall wpa_supplicant 2> /dev/null
    return 0
}

##
# wifi_if_up - set up ip link
# return:      0 => succeed OR $WIFICONN_EC_NORMAL => failed
##
function wifi_if_up()
{
    ip link set dev $wifi_if up
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_UP_IF $wifi_if
        return $WIFICONN_EC_NORMAL
    fi
    return 0
}

##
# wifi_if_down - set down ip link
# return:        0 => succeed OR $WIFICONN_EC_NORMAL => failed
# @ this function will flush ip address
##
function wifi_if_down()
{
    ip address flush dev $wifi_if
    ip link set dev $wifi_if down
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_DOWN_IF $wifi_if
        return $WIFICONN_EC_NORMAL
    fi
    return 0
}

##
# scan_and_makeup_tmpfile - scan wlan information and write out into
#                           a temp file
# return:                   0 => succeed OR $WIFICONN_EC_NORMAL => failed
##
function scan_and_makeup_tmpfile()
{
    wpa_cli -i $wifi_if scan
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_WPA_SCAN
        return $WIFICONN_EC_NORMAL
    fi

    wpa_cli -i $wifi_if scan_results > $wifi_scan_results
    return 0
}

##
# makeup_passphrase_for_ssid - read password from stdin and makeup
#                              passphrase,write the information into
#                              config file
# $1:                          ssid
# return:                      0 => succeed OR $WIFICONN_EC_NORMAL failed
##
function makeup_passphrase_for_ssid()
{
    ssid=$1
    password=""
    read -sp "password: " password
    if [[ -n $password ]]
    then
        wpa_passphrase "$ssid" "$password" > $wpa_config_dir/${ssid}.conf
        return $?
    else
        echo NONE > $wpa_config_dir/${ssid}.conf
    fi
    return 0
}

##
# get_passphrase_of_ssid - get passphrase of the given ssid from config file
# $1:                      ssid
# return:                  0 => succeed OR $WIFICONN_SSID_NOPSK => wlan no password
#                          $WIFICONN_EC_NORMAL => failed
##
function get_passphrase_of_ssid()
{
    conf_path=$wpa_config_dir/${1}.conf
    if [ ! -e $conf_path ]
    then
        perror $WIFICONN_EC_WPA_NOCONF
        return $WIFICONN_EC_NORMAL
    fi

    psk=$(grep -E '[[:space:]]psk=.+' $conf_path)
    if [ $? -ne 0 ]
    then
        grep NONE $conf_path 2>&1 > /dev/null
        if [ $? -eq 0 ]
        then
            return $WIFICONN_SSID_NOPSK
        fi
        perror $WIFICONN_EC_WPA_NOPASS
        return $WIFICONN_EC_NORMAL
    fi
    current_psk=${psk##*psk=}
    return 0
}

##
# wpa_cli_set_psk - invoke wpa_cli command set_network to set psk or key_mgmt
#                   for a given network
# $1:               network id
# $2:               optional psk,if no psk,then set key_mgmt to NONE
# return:           0 OR $WIFICONN_EC_NORMAL
##
function wpa_cli_set_psk()
{
    psk=""
    if [ $# -eq 2 ]
    then
        psk=$2
    fi

    if [[ -n $psk ]]
    then
        wpa_cli -i $wifi_if set_network $1 psk $psk
        if [ $? -ne 0 ]
        then
            perror $WIFICONN_EC_WPA_SETPSK
            return $WIFICONN_EC_NORMAL
        fi
    else
        wpa_cli -i $wifi_if set_network $1 key_mgmt NONE
        if [ $? -ne 0 ]
        then
            perror $WIFICONN_EC_WPA_SETKEYMGMT
            return $WIFICONN_EC_NORMAL
        fi
    fi

    return 0
}

##
# wpa_add_network - invoke wpa_cli command add_network to add a new network
# return:           network id OR $WIFICONN_EC_NORMAL => failed
##
function wpa_add_network()
{
    ret=$(wpa_cli -i $wifi_if add_network)
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_WPA_ADDNET
        return $WIFICONN_EC_NORMAL
    fi
    return $ret
}

##
# wpa_remove_network - remove a network
# $1:                  netwokr id
# return:              0 OR $WIFICONN_EC_NORMAL
##
function wpa_remove_network()
{
    network_id=$1
    wpa_cli -i $wifi_if remove_network $network_id
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_WPA_DELNET $network_id
        return $WIFICONN_EC_NORMAL
    fi
    return 0
}

function wifi_associate_common()
{
    ssid=$1
    nopsk=$2

    wpa_add_network
    network_id=$?
    if [ $network_id -eq $WIFICONN_EC_NORMAL ]
    then
        return $WIFICONN_EC_NORMAL
    fi
    
    wpa_cli -i $wifi_if set_network $network_id ssid \"$ssid\"
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_WPA_SETSSID $ssid
        wpa_remove_network $network_id
        return $WIFICONN_EC_NORMAL
    fi
    
    ret=0
    if [ $nopsk ]
    then
        wpa_cli_set_psk $network_id
        ret=$?
    else
        wpa_cli_set_psk $network_id $current_psk
        ret=$?
    fi

    if [ $ret -ne 0 ]
    then
        wpa_remove_network $network_id
        return $ret
    fi

    wpa_cli -i $wifi_if enable_network $network_id
    if [ $? -ne 0 ]
    then
        perror $WIFICONN_EC_WPA_ONNET $ssid
        wpa_remove_network $network_id
        return $WIFICONN_EC_NORMAL
    fi

    return 0
}

##
# wifi_associate_to_ssid_nopsk - associate interface with a wlan without psk,
#                                and enable the network if succeed
# $1:                            ssid
# return:                        0 OR $WIFICONN_EC_NORMAL
##
function wifi_associate_to_ssid_nopsk()
{
    ssid=$1
    wifi_associate_common $ssid 1
    return $?
}

## wifi_associate_to_ssid - have psk version ##
function wifi_associate_to_ssid()
{
    ssid=$1
    wifi_associate_common $ssid 0
    return $?
}

## reset_all - just reset everything ##
function reset_all()
{
    stop_wpa_supplicant
    wifi_if_down
    dhcp_off
    return 0
}

##
# get_network_id_by_ssid - get network id which associated with the given ssid
# $1:                      ssid
# return:                  network id OR $WIFICONN_EC_NORMAL => failed
##
function get_network_id_by_ssid()
{
    ssid=$1
    network_id=$(wpa_cli -i $wifi_if list_networks | \
        awk -vSSID=$ssid 'BEGIN { getline; id = 127; } { if ($2 == SSID) id = $1; } END { printf "%d",id; }')
    if [ $network_id -eq $WIFICONN_EC_NORMAL ]
    then
        perror $WIFICONN_EC_WPA_GETID $ssid
        return $WIFICONN_EC_NORMAL
    fi
    return $network_id
}

# disable ARE msg in kernel message buffer #
function disable_ARE_pcireport()
{
    # PCI_EXP_DEVCTL 8
    # PCI_EXP_DEVCTL_CERE 0x0001
    # PCI_EXP_AER_FLAGS PCI_EXP_DEVCTL_CERE(1) | PCI_EXP_DEVCTL_NFERE(2) |
    #                   PCI_EXP_DEVCTL_FERE(4) | PCI_EXP_DEVCTL_URRE(8)
    setpci -v -d 8086:a114 CAP_EXP+0x08.w=0x000e
    return 0     #^information get by 'lshw' command
}

## dhclient control ##
function dhcp_on()
{
    dhclient -v $wifi_if
    return $?
}

function dhcp_off()
{
    killall dhclient 2> /dev/null
    ip address flush dev $wifi_if
    return 0
}

## main commands ##
function wifi_up()
{
    wifi_if_up
    if [ $? -ne 0 ]
    then
        return $WIFICONN_EC_NORMAL
    fi

    start_wpa_supplicant
    if [ $? -ne 0 ]
    then
        return $WIFICONN_EC_NORMAL
    fi

    return 0
}

function wifi_down()
{
    dhcp_off
    stop_wpa_supplicant
    wifi_if_down
    if [ $? -ne 0 ]
    then
        return $WIFICONN_EC_NORMAL
    fi

    return $?
}

function wifi_scan()
{
    scan_and_makeup_tmpfile
    if [ $? -ne 0 ]
    then
        return $WIFICONN_EC_NORMAL
    fi
    echo "Scanning results from $wifi_scan_results -"
    cat $wifi_scan_results
    return 0
}

function wifi_connect_common()
{
    ssid=$1
    get_passphrase_of_ssid $ssid
    ret=$?
    if [ $ret -eq $WIFICONN_SSID_NOPSK ]
    then
        wifi_associate_to_ssid_nopsk $ssid
        ret=$?
    elif [ $ret -eq 0 ]
    then
        wifi_associate_to_ssid $ssid
        ret=$?
    fi

    if [ $ret -ne 0 ]
    then
        return $WIFICONN_EC_NORMAL
    fi

    dhcp_off
    dhcp_on

    return $ret
}

function wifi_connect()
{
    ssid=$1
    makeup_passphrase_for_ssid $ssid
    wifi_connect_common $ssid
    if [ $? -eq 0 ]
    then
        echo $ssid > $wifi_LRU
        return 0
    fi
    return $WIFICONN_EC_NORMAL
}

function wifi_disconnect()
{
    ssid=$(cat $wifi_LRU)
    get_network_id_by_ssid $ssid
    network_id=$?
    if [ $network_id -ne $WIFICONN_EC_NORMAL ]
    then
        dhcp_off
        wpa_remove_network $network_id
        return $?
    fi
    return $WIFICONN_EC_NORMAL
}

function wifi_quick_connect()
{
    ssid=$(cat $wifi_LRU)
    wifi_connect_common $ssid
    return $?
}

function wifi_status()
{
    iw dev $wifi_if link
    return 0
}

function wifi_reset()
{
    reset_all
    return 0
}

function wifi_help()
{
    echo "wifi_connect: usage: wifi_connect <interface> [command [command parameter]]"
    echo -ne "\t commands: up down scan connect disconnect quick_connect status reset help\n"
    echo -ne "\t\t up - up interface,start wpa_supplicant daemon\n"
    echo -ne "\t\t down - down interface,stop wpa_supplicant daemon\n"
    echo -ne "\t\t scan - scan and print wlan information\n"
    echo -ne "\t\t connect <SSID> - connect to wlan $SSID\n"
    echo -ne "\t\t disconnect - disconnect current wlan\n"
    echo -ne "\t\t quick_connect - connect to the last connected wlan\n"
    echo -ne "\t\t status - print state of current interface\n"
    echo -ne "\t\t reset - reset all\n"
    echo -ne "\t\t help - help information\n"
}

function main()
{
    if [ $# -lt 2 ]
    then
        perror $WIFICONN_EC_BADCMD
        return $WIFICONN_EC_NORMAL
    fi

    # set interface
    wifi_if=$1

    shift

    # get command
    cmd=$1

    shift

    ret=0
    case $cmd in
        up)
            wifi_up
            ret=$?
            ;;
        down)
            wifi_down
            ret=$?
            ;;
        scan)
            wifi_scan
            ret=$?
            ;;
        connect)
            wifi_connect $@
            ret=$?
            disable_ARE_pcireport
            ;;
        disconnect)
            wifi_disconnect
            ret=$?
            ;;
        quick_connect)
            wifi_quick_connect
            ret=$?
            disable_ARE_pcireport
            ;;
        status)
            wifi_status
            ;;
        reset)
            wifi_reset
            ;;
        help)
            wifi_help
            ret=$?
            ;;
        *)
            perror $WIFICONN_EC_BADCMD $cmd
            ret=$WIFICONN_EC_NORMAL
            ;;
    esac

    return $ret
}


main $@

exit $?
