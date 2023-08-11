export function dateTime(time) {
  return function () {
    return time.dateTime;
  };
}

export function setDateTime(dateTime) {
  return function (time) {
    return function () {
      time.dateTime = dateTime;
    };
  };
}
