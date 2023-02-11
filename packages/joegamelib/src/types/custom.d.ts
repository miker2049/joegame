// export type PackType = Record<
//   string,
//   { files: Array<{ type: string; key: string; url?: string }> }
// > & { meta: any }

export type PackType = {
  [k: string]:
    | { files: { type: string; key: string; url?: string }[] }
    | { url: string }
}
