import type { PackageDailyDownloadPoint, PackageVersionDownloadPoint } from "@/lib/types"
import {
  Bar,
  BarChart,
  CartesianGrid,
  Line,
  LineChart,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis,
} from "recharts"

interface Props {
  dailyDownloads: PackageDailyDownloadPoint[]
  versionDownloads: PackageVersionDownloadPoint[]
}

const date = new Intl.DateTimeFormat("en", {
  month: "short",
  day: "numeric",
  timeZone: "UTC",
})
const number = new Intl.NumberFormat("en-US")

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

export default function PackageDownloadsChart({ dailyDownloads, versionDownloads }: Props) {
  const recentVersions = versionDownloads.slice(0, 12)

  return (
    <div className="grid gap-4 lg:grid-cols-[minmax(0,1.2fr)_minmax(280px,0.8fr)]">
      <div className="grid gap-2">
        <div className="text-sm text-muted-foreground">Last 30 days</div>
        <div className="h-56 w-full">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={dailyDownloads} margin={{ top: 8, right: 8, left: 8, bottom: 0 }}>
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
                formatter={(value) => number.format(Number(value ?? 0))}
                labelFormatter={(value) => formatDay(String(value))}
              />
              <Line
                type="monotone"
                dataKey="download_count"
                name="Installs"
                stroke="var(--chart-3)"
                strokeWidth={2}
                dot={false}
                activeDot={{ r: 4 }}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </div>

      <div className="grid gap-2">
        <div className="text-sm text-muted-foreground">By version</div>
        <div className="h-56 w-full">
          <ResponsiveContainer width="100%" height="100%">
            <BarChart
              data={[...recentVersions].reverse()}
              layout="vertical"
              margin={{ top: 8, right: 8, left: 8, bottom: 0 }}
            >
              <CartesianGrid stroke="var(--border)" horizontal={false} />
              <XAxis
                type="number"
                allowDecimals={false}
                tickFormatter={formatCompactNumber}
                tickLine={false}
                axisLine={false}
                tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
              />
              <YAxis
                type="category"
                dataKey="version"
                tickLine={false}
                axisLine={false}
                width={72}
                tick={{ fill: "var(--muted-foreground)", fontSize: 12 }}
              />
              <Tooltip
                formatter={(value) => number.format(Number(value ?? 0))}
                labelFormatter={(value) => `v${String(value)}`}
              />
              <Bar
                dataKey="download_count"
                name="Installs"
                fill="var(--chart-2)"
                radius={[3, 3, 3, 3]}
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </div>
    </div>
  )
}
