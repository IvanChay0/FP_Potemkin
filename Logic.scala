// Logic.scala
//логика нет ввода/вывода, нет мутаций

object GreenhouseLogic:

  // Reader проверки условий (зависят от конфигурации)

  def waterNeed(plant: String, moisture: Double): Reader[Config, Boolean] =
    Reader(cfg => moisture < cfg.moistureNorm.getOrElse(plant, 0.5))

  def lightNeed(plant: String, light: Double): Reader[Config, Boolean] =
    Reader(cfg => light < cfg.minLight)

  def canHarvest(bed: Bed): Reader[Config, Boolean] =
    Reader(cfg => bed.growth >= cfg.maturityThreshold)

  //State действия над теплицей (меняют состояние)

  // Полить грядку: влага = 100%, запас воды -1
  def waterBed(id: Int): State[GreenhouseState, Unit] = State { st =>
    if st.beds.exists(_.id == id) && st.waterReserve > 0 then
      val newBeds = st.beds.map(b =>
        if b.id == id then b.copy(moisture = 100.0) else b)
      ((), st.copy(beds = newBeds, waterReserve = st.waterReserve - 1))
    else ((), st)
  }

  // Удобрить грядку рост +0.3, запас удобрений -1
  def fertilizeBed(id: Int): State[GreenhouseState, Unit] = State { st =>
    if st.beds.exists(_.id == id) && st.fertilizerReserve > 0 then
      val newBeds = st.beds.map(b =>
        if b.id == id then b.copy(growth = b.growth + 0.3) else b)
      ((), st.copy(beds = newBeds, fertilizerReserve = st.fertilizerReserve - 1))
    else ((), st)
  }

  // Включить свет на грядке свет = 100%
  def lightBed(id: Int): State[GreenhouseState, Unit] = State { st =>
    val newBeds = st.beds.map(b =>
      if b.id == id then b.copy(light = 100.0) else b)
    ((), st.copy(beds = newBeds))
  }

  // Следующий день параметры грядок меняются, день +1
  def nextDay: State[GreenhouseState, Unit] = State { st =>
    val newBeds = st.beds.map { bed =>
      bed.copy(
        moisture = math.max(0, bed.moisture - 5),
        light = math.max(0, bed.light - 3),
        growth = bed.growth + 0.15
      )
    }
    ((), st.copy(beds = newBeds, day = st.day + 1))
  }

  // Собрать урожай удалить грядку, вернуть растение
  def harvest(id: Int): State[GreenhouseState, Option[Bed]] = State { st =>
    val found = st.beds.find(_.id == id)
    if found.isDefined then
      (found, st.copy(beds = st.beds.filter(_.id != id)))
    else (None, st)
  }