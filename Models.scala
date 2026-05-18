// Models.scala
// Типы данных предметной области

// Конфигурация теплицы state
case class Config(
                   moistureNorm: Map[String, Double],  // норма влаги по типу растения
                   minLight: Double,                    // минимальный уровень света
                   growthRate: Double,                  // базовая скорость роста
                   maturityThreshold: Double            // порог зрелости для сбора
                 )

// Одна грядка с растением
case class Bed(
                id: Int,              // уникальный номер
                plantType: String,    // тип растения: "tomato", "cucumber".
                moisture: Double,     // текущая влажность %
                light: Double,        // текущий уровень света %
                growth: Double        // накопленный рост
              )

// Полное состояние теплицы
case class GreenhouseState(
                            beds: List[Bed],           // список всех грядок
                            day: Int,                  // текущий день
                            waterReserve: Int,         // запас воды
                            fertilizerReserve: Int     // запас удобрений
                          )

// Команда меню для расширения
case class Cmd(
                key: String,                           // клавиша выбора: "1", "2",
                text: String,                          // текст в меню
                action: GreenhouseState => IO[Unit]    // что выполнить при выборе
              )