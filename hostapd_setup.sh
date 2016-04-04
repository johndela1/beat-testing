iptables -t nat -A POSTROUTING -o enp0s20u1u1 -j MASQUERADE
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i wlp0s20u5u3 -o enp0s20u1u1 -j ACCEPT

killall dhcpcd 2> /dev/null
killall hostapd 2> /dev/null
sleep 1 
ip link set up dev wlp0s20u5u3
ip addr add 192.168.123.100/24 dev wlp0s20u5u3
dhcpcd  enp0s20u1u1
sleep 1 
hostapd -B /etc/hostapd/hostapd.conf
