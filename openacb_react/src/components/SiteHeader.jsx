import { useEffect, useRef, useState } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { ChevronDown, Menu, X } from 'lucide-react'
import styles from './SiteHeader.module.css'

export default function SiteHeader({ items, paths, activeTab, activeGroupId }) {
  const [openGroup, setOpenGroup] = useState(null)
  const [menuOpen, setMenuOpen] = useState(false)
  const [mobileGroup, setMobileGroup] = useState(null)
  const headerRef = useRef(null)
  const menuButtonRef = useRef(null)
  const groupButtons = useRef({})
  const location = useLocation()

  useEffect(() => {
    setOpenGroup(null)
    setMenuOpen(false)
  }, [location.pathname])

  useEffect(() => {
    const closeOutside = event => {
      if (!headerRef.current?.contains(event.target)) {
        setOpenGroup(null)
        setMenuOpen(false)
      }
    }
    document.addEventListener('pointerdown', closeOutside)
    return () => document.removeEventListener('pointerdown', closeOutside)
  }, [])

  const closeOnEscape = event => {
    if (event.key !== 'Escape') return
    if (menuOpen) {
      setMenuOpen(false)
      menuButtonRef.current?.focus()
    } else if (openGroup) {
      groupButtons.current[openGroup]?.focus()
      setOpenGroup(null)
    }
  }

  return (
    <header ref={headerRef} className={styles.header} onKeyDown={closeOnEscape} onBlur={event => {
      if (!event.currentTarget.contains(event.relatedTarget)) {
        setOpenGroup(null)
        setMenuOpen(false)
      }
    }}>
      <div className={styles.bar}>
        <Link to="/" className={styles.brand} aria-label="openACB, inicio" onClick={() => { setMenuOpen(false); setOpenGroup(null) }}>
          <img src="/openacb_nobckg.png" alt="" width="56" height="56" />
          <span className={styles.wordmark}>open<span className={styles.wordmarkAccent}>ACB</span></span>
        </Link>
        <nav className={styles.desktopNav} aria-label="Navegación principal">
          {items.map(item => {
            const active = activeGroupId === item.id || (item.single && activeTab === item.id)
            if (item.single) return <Link key={item.id} to={paths[item.id]} aria-current={active ? 'page' : undefined} className={`${styles.navLink} ${styles.projectLink}`}><span className={styles.navLabel}>{item.label}</span></Link>
            return <div key={item.id} className={styles.navGroup}>
              <button ref={element => { groupButtons.current[item.id] = element }} type="button" className={`${styles.navLink} ${active ? styles.activeGroup : ''}`} aria-current={active ? 'true' : undefined} aria-expanded={openGroup === item.id} aria-controls={`desktop-menu-${item.id}`} onClick={() => setOpenGroup(current => current === item.id ? null : item.id)}>
                <span className={styles.navLabel}>{item.label}</span><ChevronDown aria-hidden="true" size={14} />
              </button>
              {openGroup === item.id && <div id={`desktop-menu-${item.id}`} className={styles.dropdown}>
                <ul>{item.tabs.map(tab => <li key={tab.id}><Link to={paths[tab.id]} aria-current={activeTab === tab.id ? 'page' : undefined} onClick={() => setOpenGroup(null)}>{tab.label}</Link></li>)}</ul>
              </div>}
            </div>
          })}
        </nav>
        <button ref={menuButtonRef} type="button" className={styles.menuToggle} aria-label={menuOpen ? 'Cerrar menú de navegación' : 'Abrir menú de navegación'} aria-expanded={menuOpen} aria-controls="mobile-navigation" onClick={() => {
          setMenuOpen(value => !value)
          setMobileGroup(activeGroupId)
        }}>
          <span>Menú</span>{menuOpen ? <X aria-hidden="true" size={20} /> : <Menu aria-hidden="true" size={20} />}
        </button>
      </div>
      {menuOpen && <nav id="mobile-navigation" className={styles.mobileNav} aria-label="Navegación móvil">
        <div className={styles.mobileInner}>
          {items.map(item => item.single ? <Link key={item.id} to={paths[item.id]} className={styles.mobileProject} aria-current={activeTab === item.id ? 'page' : undefined}>{item.label}</Link> :
            <div key={item.id} className={styles.mobileGroup}>
              <button type="button" aria-current={activeGroupId === item.id ? 'true' : undefined} aria-expanded={mobileGroup === item.id} aria-controls={`mobile-group-${item.id}`} onClick={() => setMobileGroup(current => current === item.id ? null : item.id)}>
                {item.label}<ChevronDown aria-hidden="true" size={18} />
              </button>
              {mobileGroup === item.id && <ul id={`mobile-group-${item.id}`}>{item.tabs.map(tab => <li key={tab.id}><Link to={paths[tab.id]} aria-current={activeTab === tab.id ? 'page' : undefined} onClick={() => setMenuOpen(false)}>{tab.label}</Link></li>)}</ul>}
            </div>
          )}
        </div>
      </nav>}
    </header>
  )
}
