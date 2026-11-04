// Phones that can share files get the share sheet; desktops download (their
// share dialogs are less useful). Kept apart from chartImage.js so the button
// can pick its label without loading the exporter.
export function canShareImages() {
  if (typeof navigator === 'undefined' || typeof window === 'undefined' || !navigator.canShare) return false
  if (!window.matchMedia?.('(pointer: coarse)').matches) return false
  try {
    return navigator.canShare({ files: [new File([new Blob()], 'openacb.png', { type: 'image/png' })] })
  } catch {
    return false
  }
}
