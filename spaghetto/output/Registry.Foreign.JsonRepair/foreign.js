import jsonrepair from "jsonrepair";

export const repairImpl = (onError, onSuccess, input) => {
  try {
    const repaired = jsonrepair(input);
    return onSuccess(repaired);
  } catch (_err) {
    return onError;
  }
};
