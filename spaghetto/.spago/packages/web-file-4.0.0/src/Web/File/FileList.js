export function length(fileList) { return fileList.length; }

export function _item(index) {
  return function (fileList) {
    return fileList.item(index);
  };
}
