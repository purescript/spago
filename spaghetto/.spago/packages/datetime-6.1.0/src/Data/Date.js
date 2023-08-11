var createDate = function (y, m, d) {
  var date = new Date(Date.UTC(y, m, d));
  if (y >= 0 && y < 100) {
    date.setUTCFullYear(y);
  }
  return date;
};

export function canonicalDateImpl(ctor, y, m, d) {
  var date = createDate(y, m - 1, d);
  return ctor(date.getUTCFullYear())(date.getUTCMonth() + 1)(date.getUTCDate());
}

export function calcWeekday(y, m, d) {
  return createDate(y, m - 1, d).getUTCDay();
}

export function calcDiff(y1, m1, d1, y2, m2, d2) {
  var dt1 = createDate(y1, m1 - 1, d1);
  var dt2 = createDate(y2, m2 - 1, d2);
  return dt1.getTime() - dt2.getTime();
}
