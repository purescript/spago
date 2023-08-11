var createUTC = function (y, mo, d, h, m, s, ms) {
  var date = new Date(Date.UTC(y, mo, d, h, m, s, ms));
  if (y >= 0 && y < 100) {
    date.setUTCFullYear(y);
  }
  return date.getTime();
};

export function calcDiff(rec1, rec2) {
  var msUTC1 = createUTC(rec1.year, rec1.month - 1, rec1.day, rec1.hour, rec1.minute, rec1.second, rec1.millisecond);
  var msUTC2 = createUTC(rec2.year, rec2.month - 1, rec2.day, rec2.hour, rec2.minute, rec2.second, rec2.millisecond);
  return msUTC1 - msUTC2;
}

export function adjustImpl(just) {
  return function (nothing) {
    return function (offset) {
      return function (rec) {
        var msUTC = createUTC(rec.year, rec.month - 1, rec.day, rec.hour, rec.minute, rec.second, rec.millisecond);
        var dt = new Date(msUTC + offset);
        return isNaN(dt.getTime()) ? nothing : just({
          year: dt.getUTCFullYear(),
          month: dt.getUTCMonth() + 1,
          day: dt.getUTCDate(),
          hour: dt.getUTCHours(),
          minute: dt.getUTCMinutes(),
          second: dt.getUTCSeconds(),
          millisecond: dt.getUTCMilliseconds()
        });
      };
    };
  };
}
