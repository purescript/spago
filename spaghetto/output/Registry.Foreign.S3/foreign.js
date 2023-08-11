import aws from "aws-sdk";

export const connectImpl = ({ key, secret }, endpoint) => {
  const spacesEndpoint = new aws.Endpoint(endpoint);
  return new aws.S3({
    endpoint: spacesEndpoint,
    accessKeyId: key,
    secretAccessKey: secret,
  });
};

// TODO: we should switch to v3 of the SDK:
// https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-s3/classes/listobjectsv2command.html
export const listObjectsImpl = (s3, params) => {
  return new Promise(function (resolve, reject) {
    s3.listObjectsV2(params, function (err, data) {
      if (err) {
        reject(err);
      } else {
        resolve(data["Contents"]);
      }
    });
  });
};

export const putObjectImpl = (s3, params) => {
  return new Promise(function (resolve, reject) {
    s3.putObject(params, function (err, data) {
      if (err) {
        reject(err);
      } else {
        resolve(data);
      }
    });
  });
};

export const deleteObjectImpl = (s3, params) => {
  return new Promise(function (resolve, reject) {
    s3.deleteObject(params, function (err, data) {
      if (err) {
        reject(err);
      } else {
        resolve(data);
      }
    });
  });
};
