export default function PageHeader({ title, subtitle, scope, actions }) {
  return (
    <header className="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
      <div className="min-w-0">
        <h1 className="text-2xl font-semibold text-acb-900">{title}</h1>
        {subtitle && <p className="mt-1 text-sm text-acb-500">{subtitle}</p>}
        {scope && <p className="mt-2 text-xs font-medium text-acb-600">{scope}</p>}
      </div>
      {actions && <div className="flex shrink-0 flex-wrap items-center gap-2">{actions}</div>}
    </header>
  )
}
