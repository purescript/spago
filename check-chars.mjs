import cp from "node:child_process";

const echo = "echo";
const space = " ";
const maxCharLimit = 8191 - echo.length - space.length - 1
let initialArgs = "";
for (let index = 0; index < maxCharLimit; index++) {
  initialArgs = initialArgs + "0";  
}

const go = (args) => {
  console.log(`Attempting to spawn 'echo X' where 'X' is ${args.length} chars`);
  const result = cp.spawn(echo, [ args ], { stdio: [null, null, null]});
  result.on("close", (code, sig) => {
    if (code === 1 || sig) {
      go(args.slice(0, -1));
    } else if (code === 0) {
      console.log(`Successfully spawned with ${args.length} chars`);
    } else {
      console.log("Something else happened.");
      console.log(exit);
      console.log(sig);
    }
  });
};

go(initialArgs);
