export default function PageHeader({ title, subtitle, scope, actions }) {
  return (
    <header className="grid gap-5 border-b border-acb-300 pb-5 sm:grid-cols-[minmax(0,1fr)_auto] sm:items-end">
      <div className="min-w-0 max-w-4xl">
        <h1 className="font-display text-4xl font-semibold leading-none tracking-[-0.02em] text-acb-900 sm:text-5xl">{title}</h1>
        {subtitle && <p className="mt-3 max-w-3xl text-base leading-snug text-acb-600">{subtitle}</p>}
        {scope && <p className="mt-2 font-mono text-xs text-acb-500">{scope}</p>}
      </div>
      {actions && <div className="flex shrink-0 flex-wrap items-center gap-2 sm:justify-end">{actions}</div>}
    </header>
  )
}
