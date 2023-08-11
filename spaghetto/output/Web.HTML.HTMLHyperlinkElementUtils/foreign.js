export function href(u) {
  return function () {
    return u.href;
  };
}

export function setHref(href) {
  return function (u) {
    return function () {
      u.href = href;
    };
  };
}

// ----------------------------------------------------------------------------

export function origin(u) {
  return function () {
    return u.origin;
  };
}

// ----------------------------------------------------------------------------

export function protocol(u) {
  return function () {
    return u.protocol;
  };
}

export function setProtocol(protocol) {
  return function (u) {
    return function () {
      u.protocol = protocol;
    };
  };
}

// ----------------------------------------------------------------------------

export function username(u) {
  return function () {
    return u.username;
  };
}

export function setUsername(username) {
  return function (u) {
    return function () {
      u.username = username;
    };
  };
}

// ----------------------------------------------------------------------------

export function password(u) {
  return function () {
    return u.password;
  };
}

export function setPassword(password) {
  return function (u) {
    return function () {
      u.password = password;
    };
  };
}

// ----------------------------------------------------------------------------

export function host(u) {
  return function () {
    return u.host;
  };
}

export function setHost(host) {
  return function (u) {
    return function () {
      u.host = host;
    };
  };
}

// ----------------------------------------------------------------------------

export function hostname(u) {
  return function () {
    return u.hostname;
  };
}

export function setHostname(hostname) {
  return function (u) {
    return function () {
      u.hostname = hostname;
    };
  };
}

// ----------------------------------------------------------------------------

export function port(u) {
  return function () {
    return u.port;
  };
}

export function setPort(port) {
  return function (u) {
    return function () {
      u.port = port;
    };
  };
}

// ----------------------------------------------------------------------------

export function pathname(u) {
  return function () {
    return u.pathname;
  };
}

export function setPathname(pathname) {
  return function (u) {
    return function () {
      u.pathname = pathname;
    };
  };
}

// ----------------------------------------------------------------------------

export function search(u) {
  return function () {
    return u.search;
  };
}

export function setSearch(search) {
  return function (u) {
    return function () {
      u.search = search;
    };
  };
}

// ----------------------------------------------------------------------------

export function hash(u) {
  return function () {
    return u.hash;
  };
}

export function setHash(hash) {
  return function (u) {
    return function () {
      u.hash = hash;
    };
  };
}
