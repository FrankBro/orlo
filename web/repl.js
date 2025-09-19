import init, { repl } from "./pkg/web.js";

export class Repl {
  constructor() {
    this.ready = init(); // load wasm
  }

  async eval(command) {
    await this.ready; // wait for wasm to load
    return repl(command);
  }
}
