import { useEffect, useId, useMemo, useRef, useState } from 'react'
import { Search } from 'lucide-react'

function normalizeText(value) {
  return String(value || '')
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLocaleLowerCase('es')
}

export default function PlayerCombobox({
  options,
  value,
  onChange,
  id,
  label = 'Jugador',
  placeholder = 'Buscar jugador...',
}) {
  const generatedId = useId().replace(/:/g, '')
  const inputId = id || `player-combobox-${generatedId}`
  const listboxId = `${inputId}-listbox`
  const rootRef = useRef(null)
  const [query, setQuery] = useState('')
  const [open, setOpen] = useState(false)
  const [activeIndex, setActiveIndex] = useState(-1)

  const selectedOption = useMemo(
    () => options.find(option => String(option.value) === String(value)),
    [options, value],
  )
  const selectedLabel = selectedOption?.label || ''

  const filteredOptions = useMemo(() => {
    const search = normalizeText(query === selectedLabel ? '' : query.trim())
    if (!search) return options.slice(0, 50)

    return options.filter(option => {
      const meta = typeof option.meta === 'string' ? option.meta : ''
      return normalizeText(`${option.label} ${option.searchText || ''} ${meta}`).includes(search)
    }).slice(0, 50)
  }, [options, query, selectedLabel])

  useEffect(() => {
    setQuery(selectedLabel)
  }, [selectedLabel])

  useEffect(() => {
    const closeOnOutsideClick = event => {
      if (!rootRef.current?.contains(event.target)) {
        setOpen(false)
        setQuery(selectedLabel)
      }
    }
    document.addEventListener('mousedown', closeOnOutsideClick)
    return () => document.removeEventListener('mousedown', closeOnOutsideClick)
  }, [selectedLabel])

  useEffect(() => {
    if (!open || filteredOptions.length === 0) {
      setActiveIndex(-1)
      return
    }
    setActiveIndex(index => Math.min(Math.max(index, 0), filteredOptions.length - 1))
  }, [filteredOptions.length, open])

  const selectOption = option => {
    setQuery(option.label)
    setOpen(false)
    onChange(option)
  }

  const handleKeyDown = event => {
    if (event.key === 'ArrowDown') {
      event.preventDefault()
      setOpen(true)
      setActiveIndex(index => Math.min(index + 1, filteredOptions.length - 1))
    } else if (event.key === 'ArrowUp') {
      event.preventDefault()
      setOpen(true)
      setActiveIndex(index => index <= 0 ? filteredOptions.length - 1 : index - 1)
    } else if (event.key === 'Enter' && open && activeIndex >= 0) {
      event.preventDefault()
      selectOption(filteredOptions[activeIndex])
    } else if (event.key === 'Escape') {
      event.preventDefault()
      setOpen(false)
      setQuery(selectedLabel)
    }
  }

  return (
    <div ref={rootRef} className="relative min-w-[220px] flex-1">
      <label htmlFor={inputId} className="field-label">{label}</label>
      <div className="relative mt-1">
        <Search className="pointer-events-none absolute left-3 top-1/2 h-4 w-4 -translate-y-1/2 text-acb-400" aria-hidden="true" />
        <input
          id={inputId}
          type="text"
          role="combobox"
          aria-autocomplete="list"
          aria-expanded={open}
          aria-controls={listboxId}
          aria-activedescendant={activeIndex >= 0 ? `${inputId}-option-${activeIndex}` : undefined}
          autoComplete="off"
          value={query}
          onFocus={() => setOpen(true)}
          onChange={event => {
            setQuery(event.target.value)
            setOpen(true)
            setActiveIndex(0)
          }}
          onKeyDown={handleKeyDown}
          placeholder={placeholder}
          className="form-control pl-10"
        />
      </div>

      {open && (
        <ul
          id={listboxId}
          role="listbox"
          aria-label={label}
          className="absolute z-50 mt-1 max-h-64 w-full overflow-y-auto rounded-lg border border-acb-200 bg-white py-1 shadow-lg"
        >
          {filteredOptions.length > 0 ? filteredOptions.map((option, index) => {
            const selected = String(option.value) === String(value)
            const active = index === activeIndex
            return (
              <li
                id={`${inputId}-option-${index}`}
                key={option.value}
                role="option"
                aria-selected={selected}
                className={active || selected ? 'bg-accent-50' : undefined}
                onMouseEnter={() => setActiveIndex(index)}
              >
                <button
                  type="button"
                  tabIndex={-1}
                  onMouseDown={event => event.preventDefault()}
                  onClick={() => selectOption(option)}
                  className="flex w-full items-center justify-between gap-3 px-4 py-2 text-left text-sm text-acb-800 hover:bg-accent-50"
                >
                  <span className={selected ? 'font-semibold' : 'font-medium'}>{option.label}</span>
                  {option.meta && <span className="shrink-0 text-xs text-acb-400">{option.meta}</span>}
                </button>
              </li>
            )
          }) : (
            <li className="px-4 py-3 text-sm text-acb-500" role="option" aria-disabled="true">
              No se encontraron jugadores
            </li>
          )}
        </ul>
      )}
    </div>
  )
}
