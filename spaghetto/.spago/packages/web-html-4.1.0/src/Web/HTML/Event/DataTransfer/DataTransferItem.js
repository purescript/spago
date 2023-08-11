export function _kind(nothing, just, text, file, dataTransferItem) {
  if (dataTransferItem.kind === "string") {
    return just(text);
  } else if (dataTransferItem.kind === "file") {
    return just(file);
  } else {
    return nothing;
  }
}

export function type_(dataTransferItem) {
  return dataTransferItem.type;
}

export function _dataTransferItem(index) {
  return function (dataTransferItemList) {
    return dataTransferItemList[index];
  };
}

export function _length(dataTransferItemList) {
  return dataTransferItemList.length;
}
