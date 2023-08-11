// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Promise from "../Control.Promise/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Effect_Aff_Class from "../Effect.Aff.Class/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var Private = /* #__PURE__ */ (function () {
    function Private() {

    };
    Private.value = new Private();
    return Private;
})();
var PublicRead = /* #__PURE__ */ (function () {
    function PublicRead() {

    };
    PublicRead.value = new PublicRead();
    return PublicRead;
})();
var putObject = function (dictMonadAff) {
    var Monad0 = (dictMonadAff.MonadEffect0()).Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    return function (space) {
        return function (params) {
            var jsACL = (function () {
                if (params.acl instanceof Private) {
                    return "private";
                };
                if (params.acl instanceof PublicRead) {
                    return "public-read";
                };
                throw new Error("Failed pattern match at Registry.Foreign.S3 (line 82, column 13 - line 84, column 34): " + [ params.acl.constructor.name ]);
            })();
            var jsParams = {
                Bucket: space.bucket,
                Key: params.key,
                Body: params.body,
                ACL: jsACL
            };
            return bind(liftAff(Control_Promise.toAffE(function () {
                return $foreign.putObjectImpl(space.conn, jsParams);
            })))(function (res) {
                return pure({
                    eTag: res.ETag
                });
            });
        };
    };
};
var listObjects = function (dictMonadAff) {
    var Monad0 = (dictMonadAff.MonadEffect0()).Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    var Applicative0 = Monad0.Applicative0();
    var $$for = Data_Traversable["for"](Applicative0)(Data_Traversable.traversableArray);
    var pure = Control_Applicative.pure(Applicative0);
    return function (space) {
        return function (params) {
            var jsParams = {
                Bucket: space.bucket,
                Prefix: params.prefix
            };
            return bind(liftAff(Control_Promise.toAffE(function () {
                return $foreign.listObjectsImpl(space.conn, jsParams);
            })))(function (jsObjs) {
                return $$for(jsObjs)(function (obj) {
                    return pure({
                        key: obj.Key,
                        size: obj.Size,
                        eTag: obj.ETag
                    });
                });
            });
        };
    };
};
var deleteObject = function (dictMonadAff) {
    var Monad0 = (dictMonadAff.MonadEffect0()).Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    return function (space) {
        return function (params) {
            var jsParams = {
                Bucket: space.bucket,
                Key: params.key
            };
            return bind(liftAff(Control_Promise.toAffE(function () {
                return $foreign.deleteObjectImpl(space.conn, jsParams);
            })))(function (result) {
                return pure({
                    deleteMarker: result.DeleteMarker,
                    versionId: result.VersionId,
                    requestCharged: result.RequestCharged
                });
            });
        };
    };
};
var connect = function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    return function (key) {
        return function (region) {
            return function (bucket) {
                return bind(liftEffect(function () {
                    return $foreign.connectImpl(key, region);
                }))(function (conn) {
                    return pure({
                        bucket: bucket,
                        conn: conn
                    });
                });
            };
        };
    };
};
export {
    Private,
    PublicRead,
    connect,
    deleteObject,
    listObjects,
    putObject
};
//# sourceMappingURL=index.js.map
