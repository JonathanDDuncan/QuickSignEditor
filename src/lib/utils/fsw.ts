// Pure function: isValidFSW
// Checks if a string is a valid Formal SignWriting (FSW) sign string
// See https://tools.ietf.org/id/draft-slevinski-formal-signwriting-07.html for FSW spec

export function isValidFSW(input: any): boolean {
  if (typeof input !== 'string' || !input.trim()) return false;
  // Basic FSW sign pattern: starts with 'M' or 'S', contains only allowed chars
  // This is a simplified check for demo purposes
  const FSW_SIGN_REGEX = /^[MS][0-9a-fA-F]+(x[0-9a-fA-F]+)*$/;
  return FSW_SIGN_REGEX.test(input.trim());
}

// For Vitest compatibility, also export as default
export default isValidFSW;
