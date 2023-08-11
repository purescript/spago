// global exports
var createDate = function(y, m, d, h, mi, s, ms) {
  var date = new Date(Date.UTC(y, m, d, h, mi, s, ms));
  if (y >= 0 && y < 100) {
    date.setUTCFullYear(y);
  }
  return date;
};

var createLocalDate = function(y, m, d, h, mi, s, ms) {
  var date = new Date(y, m, d, h, mi, s, ms);
  if (y >= 0 && y < 100) {
    date.setFullYear(y);
  }
  return date;
};

export function now() {
  return new Date();
}

export function isValid(date) {
  return !isNaN(date.getTime());
}

export function toInstantImpl(just) {
  return function(nothing) {
    return function(date) {
      var t = date.getTime();
      return isNaN(t) ? nothing : just(t);
    };
  };
}

export function fromInstant(instant) {
  return new Date(instant);
}

export function jsdate(parts) {
  return createDate(
    parts.year,
    parts.month,
    parts.day,
    parts.hour,
    parts.minute,
    parts.second,
    parts.millisecond
  );
}

export function jsdateLocal(parts) {
  return function() {
    return createLocalDate(
      parts.year,
      parts.month,
      parts.day,
      parts.hour,
      parts.minute,
      parts.second,
      parts.millisecond
    );
  };
}

export function dateMethod(method, date) {
  return date[method]();
}

export function dateMethodEff(method, date) {
  return function() {
    return date[method]();
  };
}

export function parse(dateString) {
  return function() {
    return new Date(dateString);
  };
}

export function fromTime(time) {
  return new Date(time);
}
