// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Node_Buffer from "../Node.Buffer/index.js";
import * as Node_Buffer_Class from "../Node.Buffer.Class/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Node_FS from "../Node.FS/index.js";
import * as Node_FS_Constants from "../Node.FS.Constants/index.js";
import * as Node_FS_Perms from "../Node.FS.Perms/index.js";
import * as Node_FS_Stats from "../Node.FS.Stats/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Either.functorEither);
var show = /* #__PURE__ */ Data_Show.show(Node_Encoding.showEncoding);
var div = /* #__PURE__ */ Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt);
var size = /* #__PURE__ */ Node_Buffer_Class.size(Node_Buffer.mutableBufferEffect);
var handleCallback = function (cb) {
    return function (err, a) {
        var v = Data_Nullable.toMaybe(err);
        if (v instanceof Data_Maybe.Nothing) {
            return cb(new Data_Either.Right(a))();
        };
        if (v instanceof Data_Maybe.Just) {
            return cb(new Data_Either.Left(v.value0))();
        };
        throw new Error("Failed pattern match at Node.FS.Async (line 66, column 43 - line 68, column 30): " + [ v.constructor.name ]);
    };
};
var link = function (src) {
    return function (dst) {
        return function (cb) {
            return function () {
                return $foreign.linkImpl(src, dst, handleCallback(cb));
            };
        };
    };
};
var lstat = function (file) {
    return function (cb) {
        return function () {
            return $foreign.lstatImpl(file, handleCallback((function () {
                var $17 = map(Node_FS_Stats.Stats.create);
                return function ($18) {
                    return cb($17($18));
                };
            })()));
        };
    };
};
var mkdir$prime = function (file) {
    return function (v) {
        return function (cb) {
            return function () {
                return $foreign.mkdirImpl(file, {
                    recursive: v.recursive,
                    mode: Node_FS_Perms.permsToString(v.mode)
                }, handleCallback(cb));
            };
        };
    };
};
var mkdir = function (path) {
    return mkdir$prime(path)({
        recursive: false,
        mode: Node_FS_Perms.mkPerms(Node_FS_Perms.all)(Node_FS_Perms.all)(Node_FS_Perms.all)
    });
};
var mkdtemp$prime = function (prefix) {
    return function (encoding) {
        return function (cb) {
            return function () {
                return $foreign.mkdtempImpl(prefix, Node_Encoding.encodingToNode(encoding), handleCallback(cb));
            };
        };
    };
};
var mkdtemp = function (prefix) {
    return mkdtemp$prime(prefix)(Node_Encoding.UTF8.value);
};
var readFile = function (file) {
    return function (cb) {
        return function () {
            return $foreign.readFileImpl(file, {}, handleCallback(cb));
        };
    };
};
var readTextFile = function (encoding) {
    return function (file) {
        return function (cb) {
            return function () {
                return $foreign.readFileImpl(file, {
                    encoding: show(encoding)
                }, handleCallback(cb));
            };
        };
    };
};
var readdir = function (file) {
    return function (cb) {
        return function () {
            return $foreign.readdirImpl(file, handleCallback(cb));
        };
    };
};
var readlink = function (path) {
    return function (cb) {
        return function () {
            return $foreign.readlinkImpl(path, handleCallback(cb));
        };
    };
};
var realpath = function (path) {
    return function (cb) {
        return function () {
            return $foreign.realpathImpl(path, {}, handleCallback(cb));
        };
    };
};
var realpath$prime = function (path) {
    return function (cache) {
        return function (cb) {
            return function () {
                return $foreign.realpathImpl(path, cache, handleCallback(cb));
            };
        };
    };
};
var rename = function (oldFile) {
    return function (newFile) {
        return function (cb) {
            return function () {
                return $foreign.renameImpl(oldFile, newFile, handleCallback(cb));
            };
        };
    };
};
var rm$prime = function (path) {
    return function (opts) {
        return function (cb) {
            return function () {
                return $foreign.rmImpl(path, opts, handleCallback(cb));
            };
        };
    };
};
var rm = function (path) {
    return rm$prime(path)({
        force: false,
        maxRetries: 100,
        recursive: false,
        retryDelay: 1000
    });
};
var rmdir$prime = function (path) {
    return function (opts) {
        return function (cb) {
            return function () {
                return $foreign.rmdirImpl(path, opts, handleCallback(cb));
            };
        };
    };
};
var rmdir = function (path) {
    return function (cb) {
        return rmdir$prime(path)({
            maxRetries: 0,
            retryDelay: 100
        })(cb);
    };
};
var stat = function (file) {
    return function (cb) {
        return function () {
            return $foreign.statImpl(file, handleCallback((function () {
                var $19 = map(Node_FS_Stats.Stats.create);
                return function ($20) {
                    return cb($19($20));
                };
            })()));
        };
    };
};
var symlink = function (src) {
    return function (dest) {
        return function (ty) {
            return function (cb) {
                return function () {
                    return $foreign.symlinkImpl(src, dest, Node_FS.symlinkTypeToNode(ty), handleCallback(cb));
                };
            };
        };
    };
};
var truncate = function (file) {
    return function (len) {
        return function (cb) {
            return function () {
                return $foreign.truncateImpl(file, len, handleCallback(cb));
            };
        };
    };
};
var unlink = function (file) {
    return function (cb) {
        return function () {
            return $foreign.unlinkImpl(file, handleCallback(cb));
        };
    };
};
var utimes = function (file) {
    return function (atime) {
        return function (mtime) {
            return function (cb) {
                var toEpochMilliseconds = function ($21) {
                    return Data_DateTime_Instant.unInstant(Data_DateTime_Instant.fromDateTime($21));
                };
                var ms = function (v) {
                    return Data_Int.round(v);
                };
                var fromDate = function (date) {
                    return div(ms(toEpochMilliseconds(date)))(1000);
                };
                return function () {
                    return $foreign.utimesImpl(file, fromDate(atime), fromDate(mtime), handleCallback(cb));
                };
            };
        };
    };
};
var writeFile = function (file) {
    return function (buff) {
        return function (cb) {
            return function () {
                return $foreign.writeFileImpl(file, buff, {}, handleCallback(cb));
            };
        };
    };
};
var writeTextFile = function (encoding) {
    return function (file) {
        return function (buff) {
            return function (cb) {
                return function () {
                    return $foreign.writeFileImpl(file, buff, {
                        encoding: show(encoding)
                    }, handleCallback(cb));
                };
            };
        };
    };
};
var fdWrite = function (fd) {
    return function (buff) {
        return function (off) {
            return function (len) {
                return function (pos) {
                    return function (cb) {
                        return function () {
                            return $foreign.writeImpl(fd, buff, off, len, Data_Nullable.toNullable(pos), handleCallback(cb));
                        };
                    };
                };
            };
        };
    };
};
var fdRead = function (fd) {
    return function (buff) {
        return function (off) {
            return function (len) {
                return function (pos) {
                    return function (cb) {
                        return function () {
                            return $foreign.readImpl(fd, buff, off, len, Data_Nullable.toNullable(pos), handleCallback(cb));
                        };
                    };
                };
            };
        };
    };
};
var fdOpen = function (file) {
    return function (flags) {
        return function (mode) {
            return function (cb) {
                return function () {
                    return $foreign.openImpl(file, Node_FS_Constants.fileFlagsToNode(flags), Data_Nullable.toNullable(mode), handleCallback(cb));
                };
            };
        };
    };
};
var fdNext = function (fd) {
    return function (buff) {
        return function (cb) {
            return function __do() {
                var sz = size(buff)();
                return fdRead(fd)(buff)(0)(sz)(Data_Maybe.Nothing.value)(cb)();
            };
        };
    };
};
var fdClose = function (fd) {
    return function (cb) {
        return function () {
            return $foreign.closeImpl(fd, handleCallback(cb));
        };
    };
};
var fdAppend = function (fd) {
    return function (buff) {
        return function (cb) {
            return function __do() {
                var sz = size(buff)();
                return fdWrite(fd)(buff)(0)(sz)(Data_Maybe.Nothing.value)(cb)();
            };
        };
    };
};
var copyFile$prime = function (src) {
    return function (dest) {
        return function (mode) {
            return function (cb) {
                return function () {
                    return $foreign.copyFileImpl(src, dest, mode, handleCallback(cb));
                };
            };
        };
    };
};
var copyFile = function (src) {
    return function (dest) {
        return copyFile$prime(src)(dest)(Node_FS_Constants.defaultCopyMode);
    };
};
var chown = function (file) {
    return function (uid) {
        return function (gid) {
            return function (cb) {
                return function () {
                    return $foreign.chownImpl(file, uid, gid, handleCallback(cb));
                };
            };
        };
    };
};
var chmod = function (file) {
    return function (perms) {
        return function (cb) {
            return function () {
                return $foreign.chmodImpl(file, Node_FS_Perms.permsToString(perms), handleCallback(cb));
            };
        };
    };
};
var appendTextFile = function (encoding) {
    return function (file) {
        return function (buff) {
            return function (cb) {
                return function () {
                    return $foreign.appendFileImpl(file, buff, {
                        encoding: show(encoding)
                    }, handleCallback(cb));
                };
            };
        };
    };
};
var appendFile = function (file) {
    return function (buff) {
        return function (cb) {
            return function () {
                return $foreign.appendFileImpl(file, buff, {}, handleCallback(cb));
            };
        };
    };
};
var access$prime = function (path) {
    return function (mode) {
        return function (cb) {
            return function () {
                return $foreign.accessImpl(path, mode, function (err) {
                    return cb(Data_Nullable.toMaybe(err))();
                });
            };
        };
    };
};
var access = function (path) {
    return access$prime(path)(Node_FS_Constants.defaultAccessMode);
};
export {
    access,
    access$prime,
    copyFile,
    copyFile$prime,
    mkdtemp,
    mkdtemp$prime,
    rename,
    truncate,
    chown,
    chmod,
    lstat,
    stat,
    link,
    symlink,
    readlink,
    realpath,
    realpath$prime,
    unlink,
    rmdir,
    rmdir$prime,
    rm,
    rm$prime,
    mkdir,
    mkdir$prime,
    readdir,
    utimes,
    readFile,
    readTextFile,
    writeFile,
    writeTextFile,
    appendFile,
    appendTextFile,
    fdOpen,
    fdRead,
    fdNext,
    fdWrite,
    fdAppend,
    fdClose
};
//# sourceMappingURL=index.js.map
