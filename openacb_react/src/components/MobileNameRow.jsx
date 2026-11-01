// Mobile-only row that shows a table row's name (player, team, season...) above
// its stats, so the name doesn't need its own wide column. Hidden on wider
// screens (see index.css). Cells that move into this line get `mobile-name-hide`,
// and each MobileNameRow + stats row pair goes in its own `mobile-name-group` tbody.
export default function MobileNameRow({ rank, name, meta, onClick, className = '', wrap = false }) {
  return (
    <tr
      className={`mobile-name-row ${onClick ? 'mobile-name-row-link' : ''} ${className || 'bg-white'}`}
      onClick={onClick}
    >
      {/* Larger than any table here, so it spans every column; the extra columns get no width */}
      <td colSpan={100}>
        <div className={`mobile-name ${wrap ? 'mobile-name-wrap' : ''}`}>
          {rank != null && <span className="mobile-name-rank">{rank}</span>}
          <span className="mobile-name-player">{name}</span>
          {meta && <span className="mobile-name-meta">{meta}</span>}
        </div>
      </td>
    </tr>
  )
}
