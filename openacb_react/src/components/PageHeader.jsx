export default function PageHeader({ title, subtitle, scope, actions }) {
  return (
    <header className="pageHeader flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
      <div className="pageHeaderContent min-w-0">
        <h1 className="pageHeaderTitle text-2xl font-semibold text-acb-900">{title}</h1>
        {subtitle && <p className="pageHeaderSubtitle mt-1 text-sm text-acb-500">{subtitle}</p>}
        {scope && <p className="pageHeaderScope mt-2 text-xs font-medium text-acb-600">{scope}</p>}
      </div>
      {actions && <div className="pageHeaderActions flex shrink-0 flex-wrap items-center gap-2">{actions}</div>}
    </header>
  )
}
