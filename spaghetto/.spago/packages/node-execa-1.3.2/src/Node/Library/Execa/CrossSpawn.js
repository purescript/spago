import process from "process";

export const processHasChdir = () => process.chdir !== undefined;
