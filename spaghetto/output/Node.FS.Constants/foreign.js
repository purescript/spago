import { constants } from "node:fs";

export const f_OK = constants.F_OK;

export const r_OK = constants.R_OK;

export const w_OK = constants.W_OK;

export const x_OK = constants.X_OK;

export const copyFile_EXCL = constants.COPYFILE_EXCL;

export const copyFile_FICLONE = constants.COPYFILE_FICLONE;

export const copyFile_FICLONE_FORCE = constants.COPYFILE_FICLONE_FORCE;

export const appendCopyMode = (l, r) => l | r;
