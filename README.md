![](image/Pietro_Gerini.jpg)

Как понять бухучет с помощью одного тождества
=============================================

Ответ: через упражнения. Чтобы начать, выполните `Упражнение 1` в [accounting.xlsx](accounting.xlsx)

## План занятий ЦМФ

Студенты выполняют `Упражнение 1` и приносят ноутбуки с Excel.

1. Понятие фирмы, активы и пассивы, их виды. Балансовое тождество активов и пассивов. Нетто и брутто баланс.
2. Проводки как способ изменения переменных в тождестве. Корреспонденция счетов. Двойная запись.
3. Деньги, запасы, расходы и продажи. Способы оценки запасов. Момент признания расходов. 
   Изменения по счетам и остатки. (Упражнение 2.)
4. Т-счета, дебит и кредит счетов. Приницип начислений, дебиторская и кредиторская задолженность. 
   ОПУ и ОДДС. Прочие вопросы.
5. Как запрограммировать свою бухгалтерскую систему. 

Упражнения:

1. Потоки и запасы. Как доходы и расходы попали в тождество с запасами? (Упражнение 1). 
2. Проводки без T-счетов. (Упражнение 2). 
3. Проводки с T-счетами.
4. Система учета на примере accounting.py. Принципы построения, ограничения и возможности расширения.
   Архитектура учетных систем. Lessons learnt.
  

## Задачи учета

- Какое имущество есть у фирмы?
- Кому фирма должна денег?
- Получает ли фирма прибыль?
- Как фирма получает и расходует денежные средства?
- Может ли фирма выполнить свои обязательства?

## Определения

Aктивы фирмы:
   - то, что (обычно) можно пощупать 
   - то, что продав можно превратить в деньги
   - ~ инвентаризация имущества

Пассивы фирмы:
   - записи о том, кто имеет права требования на активы фирмы и в каком объеме
   - нельзя физически пощупать
   - ~ список требований

Активы равны пассивам. 

## Основное тождество

Мы можем детализировать тождество `Aктивы == Пассивы` следующим образом: 

```
Денежные средства + Запасы + Счета к получению + Здания, сооружения, оборудование 
= Капитал + (Выручка - Затраты) + Счета к оплате + Долг                           (1)
```                                                      

Все, что происходит на фирме, можно представить как изменение значений переменных в 
этом равенстве (и это называется проводки):

- равенство сломается, если что-то добавить только в одну часть `->` привет двойной записи!
- некоторые переменные показывают запасы, а некоторые потоки  `->` помним, что 
  поток за период - это запас (открыли кран в ванну на час)
- мы можем перенести значения с минусом в другую сторону равнества `->` привет, брутто баланс!
- мы бы хотели знать за счет чего увеличивались и уменьшилась денежные средства
  `->` привет, отчет о движении денежных средств!
- а точно тут ничего не завышается `->` привет, принцип консервативности!
- что вообще считать прибылью `->` привет, принцип начислений!  
- а какие именно запасы ушли в расходы? `->` здравствуй, учетная политика!

### Промежуточное задание

- Посмотрите на тождество 1. Сколько проводок (изменений переменных попарно) возможно в приницпе? Как из этих пар переменных наиболее важны для описания деятельности фирмы?


## Список счетов
 
| Тип |  Название            | Как еще называется               | Номера счетов |
| :-: | :------------------- | :------------------------------- | :-----------: |
|  А  | Денежные средства    |                                  |               |
|  А  | Запасы               |                                  |               |
|  А  | Счета к получению    | Дебиторская задолженность        |               |
|  А  | Здания, сооружения, оборудование   |  Основные средства |               |
|  А  | Текущие расходы      |  Затраты                         |               |
|  П  | Взносы в капитал     |                                  |               |
|  П  | Накопленная прибыль  |                                  |               |
|  П  | Текущие доходы       |  Выручка                         |               |
|  П  | Счета к оплате       |  Кредиторская задолженность      |               |
|  П  | Долг                 |  Заемные средства                |               |


См. [План счетов](http://www.consultant.ru/document/cons_doc_LAW_29165/)

## Пример проще некуда

Создана фирма с единственной целью - купить и перепродать ноутбук. Убираем переменные, которых нет в примере: 

```
Денежные средства + Запасы + Расходы =  Капитал + Доходы 
```

1. Создали фирму: ```Капитал +40, Наличность +40```
2. Купили ноутбук для перепродажи: ```Товары +30, Наличность -30```
3. Продали этот ноутбук дороже: ```Доходы +35, Деньги +35```
                                ```Расходы +30, Товары -30```
4. Посчитали прибыль ```=5```


На самом деле проводки в торговой фирме идут [примерно так](https://glavkniga.ru/situations/k503023).

## Хитрости

1. Для представления баланса часть счетов сальдируется (брутто-нетто баланс). 
2. Расходы - активный счет, доходы - пассивный. 
3. Сначала начисляем, потом платим.
4. Для избежания ошибок придумана двойная запись.
5. Двойная запись и T-счета близкие, но не одинаковые вещи.

## Двойная запись

- Двойная запись позволяет избежать ошибок в учете операций 
- Принципиально, учет возможен и без нее
- Означает, что каждая транзакция должна повлиять на два счета, 
  для сохранения тождества.


## Т-счет


```
  Д  |  К     
============
 0   |         <- входящее сальдо (дебит)
------------     
 15  |         <- увеличиваем дебит счета
     |  10     <- увеличиваем кредит счета 
     |  25    
 40  |
------------     
 20  |         <- исходящее сальдо (дебит) 
============
```

## Про вообще где-то почитать можно?

Да, долго и мучительно: 

- [Учебник Кондракова по бухучету](https://www.ozon.ru/context/detail/id/7543840/)
- [ПБУ, план счетов](http://www.consultant.ru/cons/cgi/online.cgi?req=doc&base=LAW&n=71763&rnd=EEB45027EB6807DA2E14E7C8BDD23576&dst=100003&fld=134#08337814112141273)
- [МСФО](https://www.minfin.ru/ru/perfomance/accounting/mej_standart_fo/msfo_ob/)
- [много другой литературы](https://www.klerk.ru/buh/articles/462537/)

Коротко:
- [Бухгалтерский учет для программистов, 2018](https://habr.com/ru/post/410275/)
- [Accounting for Computer Scientists, 2011](https://martin.kleppmann.com/2011/03/07/accounting-for-computer-scientists.html)

Неясно:
- [Accoutning Software Design Patterns](https://stackoverflow.com/a/163634/1758363)


## Дополнительные темы

- амортизация
- налоги
- дивиденды
- резервы (банки)


## Ссылка на colab

<https://colab.research.google.com/drive/1qhPY5MVTHnW5JawC3a09PYkjA9PI7fhS>

## Примеры кода других 

- [100daysofpython/accounting](https://github.com/danshorstein/100daysofpython/blob/master/Days19-21/accounting/accounting.py)
- ledger/hlegder
- плагин для 

## Уроки из кодирования 

- мы делаем упрощенную и ограниченную (resticted), но тем не менее 
  логически полную версию системы учета 
- двойная запись сильно упрощает реализацию
- валидация "а это праильная трансзакция" - вне системы
- интерфейсы - дорогое удовольствие (CLI tables)   
- как должна общать с внешним миром? как хранить информацию?
  какие tradeoffs?
- что в принипе отличает MWE от реальной системы?  
