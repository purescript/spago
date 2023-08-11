var createDateTime = function (y, m, d, h, mi, s, ms) {
  var dateTime = new Date(Date.UTC(y, m, d, h, mi, s, ms));
  if (y >= 0 && y < 100) {
    dateTime.setUTCFullYear(y);
  }
  return dateTime;
};

export function fromDateTimeImpl(y, mo, d, h, mi, s, ms) {
  return createDateTime(y, mo - 1, d, h, mi, s, ms).getTime();
}

export function toDateTimeImpl(ctor) {
  return function (instant) {
    var dt = new Date(instant);
    return ctor (dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds());
  };
}
