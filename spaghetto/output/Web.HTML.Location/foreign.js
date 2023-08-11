export function hash(location) {
  return function () {
    return location.hash;
  };
}

export function setHash(hash) {
  return function (location) {
    return function () {
      location.hash = hash;
    };
  };
}

// ----------------------------------------------------------------------------

export function host(location) {
  return function () {
    return location.host;
  };
}

export function setHost(host) {
  return function (location) {
    return function () {
      location.host = host;
    };
  };
}

// ----------------------------------------------------------------------------

export function hostname(location) {
  return function () {
    return location.hostname;
  };
}

export function setHostname(hostname) {
  return function (location) {
    return function () {
      location.hostname = hostname;
    };
  };
}

// ----------------------------------------------------------------------------

export function href(location) {
  return function () {
    return location.href;
  };
}

export function setHref(href) {
  return function (location) {
    return function () {
      location.href = href;
    };
  };
}

// ----------------------------------------------------------------------------

export function origin(location) {
  return function () {
    return location.origin;
  };
}

export function setOrigin(origin) {
  return function (location) {
    return function () {
      location.origin = origin;
    };
  };
}

// ----------------------------------------------------------------------------

export function pathname(location) {
  return function () {
    return location.pathname;
  };
}

export function setPathname(pathname) {
  return function (location) {
    return function () {
      location.pathname = pathname;
    };
  };
}

// ----------------------------------------------------------------------------

export function port(location) {
  return function () {
    return location.port;
  };
}

export function setPort(port) {
  return function (location) {
    return function () {
      location.port = port;
    };
  };
}

// ----------------------------------------------------------------------------

export function protocol(location) {
  return function () {
    return location.protocol;
  };
}

export function setProtocol(protocol) {
  return function (location) {
    return function () {
      location.protocol = protocol;
    };
  };
}

// ----------------------------------------------------------------------------

export function search(location) {
  return function () {
    return location.search;
  };
}

export function setSearch(search) {
  return function (location) {
    return function () {
      location.search = search;
    };
  };
}

// ----------------------------------------------------------------------------

export function assign(url) {
  return function (location) {
    return function () {
      location.assign(url);
    };
  };
}

// ----------------------------------------------------------------------------

export function replace(url) {
  return function (location) {
    return function () {
      location.replace(url);
    };
  };
}

// ----------------------------------------------------------------------------

export function reload(location) {
  return function () {
    location.reload();
  };
}
