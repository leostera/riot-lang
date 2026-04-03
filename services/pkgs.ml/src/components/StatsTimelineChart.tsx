import type { RegistryStatsActivityPoint } from "@/lib/types"
import {
  CartesianGrid,
  Line,
  LineChart,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis,
} from "recharts"

interface Props {
  data: RegistryStatsActivityPoint[]
}

const number = new Intl.NumberFormat("en-US")
const date = new Intl.DateTimeFormat("en", {
  month: "short",
  day: "numeric",
  timeZone: "UTC",
})

const series = [
  {
    key: "package_downloads",
    label: "Package installs",
    color: "var(--chart-3)",
  },
  {
    key: "riot_downloads",
    label: "Riot installs",
    color: "var(--chart-1)",
  },
  {
    key: "ocaml_downloads",
    label: "OCaml installs",
    color: "var(--chart-2)",
  },
  {
    key: "index_reads",
    label: "Index refreshes",
    color: "var(--chart-5)",
  },
  {
    key: "releases_published",
    label: "Releases",
    color: "var(--chart-4)",
  },
] as const

function formatDay(value: string): string {
  const [year, month, dayOfMonth] = value.split("-").map(Number)
  return date.format(new Date(Date.UTC(year, (month ?? 1) - 1, dayOfMonth ?? 1)))
}

function formatCompactNumber(value: number): string {
  if (value >= 1_000) {
    return `${Math.round(value / 100) / 10}k`
  }

  return number.format(value)
}

export default function StatsTimelineChart({ data }: Props) {
  return (
    <div className="grid gap-3">
      <div className="h-80 w-full">
        <ResponsiveContainer width="100%" height="100%">
          <LineChart data={data} margin={{ top: 8, right: 8, left: 8, bottom: 0 }}>
            <CartesianGrid stroke="var(--border)" vertical={false} />
            <XAxis
              dataKey="date"
              tickFormatter={formatDay}
              tickLine={false}
              axisLine={false}
              minTickGap={24}
              tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
            />
            <YAxis
              allowDecimals={false}
              tickFormatter={formatCompactNumber}
              tickLine={false}
              axisLine={false}
              width={40}
              tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
            />
            <Tooltip
              content={({ active, label, payload }) => {
                if (!active || label === undefined || payload === undefined || payload.length === 0) {
                  return null
                }

                return (
                  <div className="min-w-44 rounded-md border border-border bg-background px-3 py-2 text-sm shadow-sm">
                    <div className="mb-2 font-medium text-foreground">{formatDay(String(label))}</div>
                    <div className="grid gap-1.5">
                      {payload.map((item) => {
                        const numeric = typeof item.value === "number" ? item.value : Number(item.value ?? 0)
                        return (
                          <div key={String(item.dataKey)} className="flex items-center justify-between gap-3">
                            <div className="flex items-center gap-2 text-muted-foreground">
                              <span
                                className="inline-block h-2 w-2 rounded-full"
                                style={{ backgroundColor: item.color }}
                              />
                              <span>{item.name}</span>
                            </div>
                            <span className="font-medium text-foreground">{number.format(numeric)}</span>
                          </div>
                        )
                      })}
                    </div>
                  </div>
                )
              }}
            />
            {series.map((item) => (
              <Line
                key={item.key}
                type="monotone"
                dataKey={item.key}
                name={item.label}
                stroke={item.color}
                strokeWidth={2}
                dot={false}
                activeDot={{ r: 4 }}
              />
            ))}
          </LineChart>
        </ResponsiveContainer>
      </div>

      <div className="flex flex-wrap gap-x-4 gap-y-2 text-sm text-muted-foreground">
        {series.map((item) => (
          <div key={item.key} className="flex items-center gap-2">
            <span className="inline-block h-2 w-2 rounded-full" style={{ backgroundColor: item.color }} />
            <span>{item.label}</span>
          </div>
        ))}
      </div>
    </div>
  )
}
