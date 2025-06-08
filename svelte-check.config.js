// svelte-check config: treat unused @ts-expect-error as warning, not error
module.exports = {
  compilerOptions: {
    // No direct option for this in svelte-check, but you can use tsconfig.json
  },
  onDiagnostic: (diagnostic) => {
    if (diagnostic.code === 2578) {
      diagnostic.category = 1; // 1 = warning, 2 = error
    }
    return diagnostic;
  }
};
