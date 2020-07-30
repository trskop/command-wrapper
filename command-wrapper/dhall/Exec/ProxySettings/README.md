# Proxy Settings

Command line applications commonly use environment variables for proxy
settings.  Usage is somewhat inconsistent. Some applications use all lowercase
and some all uppercase variants of the following environment variables:


*   `http_proxy=[protocol://]<host>[:port]` and
    `HTTP_PROXY=[protocol://]<host>[:port]` is used when accessing services
    over HTTP.

*   `https_proxy=[protocol://]<host>[:port]` and
    `HTTPS_PROXY=[protocol://]<host>[:port]` is used when accessing services
    over HTTPS.

*   `ftp_proxy=[protocol://]<host>[:port]` and
    `FTP_PROXY=[protocol://]<host>[:port]` is used when accessing services
    over FTP.

*   `all_proxy=[protocol://]<host>[:port]` and
    `ALL_PROXY=[protocol://]<host>[:port]` when set it's used when there's no
    protocol-specific proxy setting available.

*   Other variants may exist as well. They usually follow this pattern:

    ```
    <url-protocol>_proxy=[protocol://]<host>[:port]
    <url-protocol>_PROXY=[protocol://]<host>[:port]
    ```

    Where `<url-protocol>` is usually one of RSYNC, FTPS, POP3, IMAP, SMTP,
    LDAP, etc.

*   `no_proxy=host[,host[...]]` and `NO_PROXY=host[,host[...]]` contains coma
    separated list of hostname, IPs, and network addresses that should be
    contacted directly, in other words, for these proxies are not used.

    ```Bash
    NO_PROXY=localhost,127.0.0.1,*.my.lan.domain,.home
    NO_PROXY=localhost,127.0.0.0/8,10.0.0.0/8,192.168.0.0/16,172.16.0.0/12,*.localdomain.com
    ```

Above descriptions mention `protocol://`, when omitted it is usually assumed
that it's `http://`. Following list includes commonly supported values (thanks
to `libcurl` being used all over the place):

*   `http://`
*   `https://`
*   `socks4://`
*   `socks4a://`
*   `socks5://`
*   `socks5h://`
