# -*- coding: utf-8 -*-

from collections import OrderedDict
import cw_funcs as cw
import helper_funcs as hf
import strategies as stg
import mechanisms as mech
import numpy as np
import os
import scipy as sc
import math
import matplotlib.pyplot as plt
from signal import signal, SIGPIPE, SIG_DFL
signal(SIGPIPE, SIG_DFL)

class AdaptPlot():
	"""
	Base class which runs the analyst strategies. 
	Compatible with the Quandaric-Strategy and the mechanisms having fixed
	dataset size n, desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
	""" 

	def __init__(self):
		pass


	def plot_from_data(self):
		plt.figure()
		# Baseline_rmse = [0.003461749999999998, 0.005167999999999998, 0.007429250000000001, 0.010434500000000003, 0.012934500000000008, 0.014819000000000004, 0.01734725, 0.019392000000000006, 0.02284150000000001, 0.02589525000000002]
		# DataSplit_rmse = [0.0052704000000000015, 0.004890399999999997, 0.005069599999999993, 0.004970399999999999, 0.005001600000000001, 0.004935199999999993, 0.0049336000000000015, 0.004716799999999999, 0.004423199999999998, 0.004393599999999999]
		# Thresh_rmse = [0.0016713223685691878, 0.0018763961342297397, 0.0026677859169896177, 0.0030729027122588528, 0.002635966009309241, 0.003368499118227935, 0.0035926994519737943, 0.004655638038143437, 0.004816376749581941, 0.004697146505462881]

		# Gauss_rmse = [0.002079523044929074, 0.002622097092175361, 0.0030713702566929186, 0.006298445045695285, 0.0037181382490414916, 0.0027625319362159966, 0.007837710847867456, 0.005537577919863043, 0.006532317258751424, 0.004855898943437798]

		# GnC_gauss_rmse = [0.01916235322528332, 0.01000000000000001, 0.009850000000000012, 0.010000000000000014, 0.008400000000000012, 0.008400000000000012, 0.007400000000000009, 0.008156000000000004, 0.008992771666666666, 0.007992771666666666]

		# GnC_thresh_rmse = [0.005343771542755635, 0.006803475877977275, 0.009531920000000011, 0.009850000000000012, 0.009000000000000011, 0.00885000000000001, 0.006550000000000009, 0.00764350000000001, 0.00783484693877551, 0.008113249999999997]

		# GnC_DataSplit_rmse = [0.00017313600000000033, 0.027749999999999997, 0.03300000000000001, 0.03125000000000001, 0.01675000000000001, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002]
		Baseline_rmse = [0.0015174000000000023, 0.002108600000000003, 0.004334199999999999, 0.0039570000000000004, 0.004679799999999998, 0.005598399999999996, 0.007080799999999996, 0.008480199999999997, 0.009256999999999996, 0.010186199999999996, 0.010660199999999996, 0.0124996, 0.013005599999999997, 0.014386199999999998, 0.0149286, 0.015471399999999996, 0.015434799999999999, 0.017612400000000007, 0.0173678, 0.017967000000000004, 0.019021600000000007, 0.019173000000000006, 0.02095540000000001, 0.021722400000000006, 0.02251800000000001, 0.02196180000000001, 0.02408160000000001, 0.02417700000000001, 0.02520100000000002, 0.025103400000000015, 0.02688680000000001, 0.0274992, 0.026683800000000014, 0.028658800000000005, 0.029518200000000015, 0.030545600000000013, 0.030925600000000004, 0.03233720000000001, 0.0331248, 0.034705799999999995, 0.035081600000000004, 0.03528800000000001, 0.03723660000000001, 0.03622700000000001, 0.0377904, 0.0383904, 0.039497199999999996, 0.03987, 0.04143619999999999, 0.043711999999999994, 0.044218600000000004, 0.043955, 0.045269199999999996, 0.045331399999999994, 0.047167, 0.0462754, 0.047538999999999984, 0.04902379999999999, 0.04932019999999999, 0.049804, 0.051012800000000004, 0.052869599999999996, 0.05171419999999999, 0.052071599999999996, 0.053575399999999995, 0.05403919999999999, 0.053905199999999986, 0.05598900000000001, 0.05818659999999999, 0.05796239999999999, 0.0584252, 0.058821599999999995, 0.0589738, 0.05959000000000001, 0.0613946, 0.061718200000000015, 0.0622988, 0.0636594, 0.06443940000000001, 0.0646096, 0.0643352, 0.06554940000000001, 0.06564660000000001, 0.0670332, 0.06790260000000001, 0.0678616, 0.0678208, 0.06966420000000001, 0.07038380000000002, 0.07164240000000001, 0.0721972, 0.0726084, 0.0737546, 0.0732742, 0.07324920000000001, 0.0738734, 0.07480960000000002, 0.07584959999999999, 0.0764934, 0.0768378, 0.07745440000000002, 0.0778154, 0.07869500000000001, 0.07869420000000002, 0.07850760000000002, 0.08000360000000001, 0.07965680000000001, 0.08070280000000002, 0.08100000000000003, 0.08096060000000002, 0.08229620000000003, 0.08295280000000001, 0.08348260000000002, 0.0836008, 0.08663740000000002, 0.08628240000000002, 0.08678000000000005, 0.08771540000000003, 0.08937040000000003, 0.08831340000000001, 0.08861940000000003, 0.09056620000000001, 0.09034980000000001, 0.09125320000000002, 0.09163240000000002, 0.09181600000000001, 0.09101640000000003, 0.09316780000000001, 0.09293259999999999, 0.09384980000000002, 0.09457, 0.09552659999999999, 0.0955688, 0.097774, 0.09792899999999999, 0.09864199999999998, 0.0992316, 0.09907939999999998, 0.09948519999999998, 0.10054439999999998, 0.1018562, 0.10223259999999998, 0.10254219999999999, 0.10342199999999999, 0.1031698, 0.10321399999999999, 0.10445940000000001, 0.1051062, 0.10569039999999999, 0.10672239999999997, 0.10761679999999996, 0.10975439999999999, 0.11026679999999997, 0.11111199999999996, 0.11058119999999998, 0.11097279999999995, 0.1121072, 0.11308099999999996, 0.11410559999999999, 0.11421199999999998, 0.11417979999999998, 0.11458819999999999, 0.11587339999999997, 0.11572260000000001, 0.11642319999999999, 0.11782059999999997, 0.11870139999999996, 0.11886219999999999, 0.12010639999999997, 0.12073979999999995, 0.12138239999999997, 0.12163999999999997, 0.122094, 0.12382259999999998, 0.12584039999999996, 0.125349, 0.12464659999999997, 0.12572779999999997, 0.12571399999999996, 0.1261842, 0.1277336, 0.1280516, 0.12939059999999997, 0.12939379999999995, 0.12942259999999997, 0.12951240000000003, 0.13050879999999998, 0.132291, 0.13167979999999996, 0.1312228, 0.13232, 0.1331448, 0.1333072, 0.134032, 0.133644, 0.1348192, 0.1343308, 0.1346244, 0.13573059999999998, 0.13655119999999998]
		DataSplit_rmse = [0.009552, 0.008559999999999998, 0.009519999999999999, 0.011215999999999997, 0.012848000000000002, 0.011568000000000002, 0.009583999999999992, 0.009935999999999995, 0.010031999999999994, 0.011536000000000001, 0.008527999999999999, 0.007984000000000002, 0.011343999999999998, 0.009552, 0.008688000000000001, 0.011087999999999997, 0.009488000000000002, 0.009424000000000002, 0.008239999999999996, 0.009743999999999997, 0.010767999999999998, 0.010927999999999998, 0.012432000000000006, 0.009968, 0.009712, 0.010351999999999997, 0.00888, 0.011024, 0.010032000000000001, 0.008815999999999996, 0.010351999999999998, 0.011375999999999989, 0.009711999999999998, 0.010543999999999996, 0.007407999999999998, 0.011472000000000003, 0.009839999999999998, 0.010352000000000002, 0.009871999999999999, 0.012112000000000005, 0.008719999999999997, 0.011568, 0.013616000000000003, 0.008751999999999994, 0.010384000000000003, 0.011119999999999996, 0.008048000000000001, 0.01176, 0.008784, 0.008848, 0.009999999999999995, 0.008432, 0.011312000000000004, 0.009104000000000001, 0.009007999999999997, 0.010287999999999995, 0.009008, 0.014096000000000006, 0.009871999999999999, 0.011759999999999998, 0.010511999999999997, 0.009648, 0.011663999999999999, 0.009903999999999998, 0.007503999999999995, 0.00872, 0.01016, 0.008432000000000002, 0.011567999999999997, 0.009615999999999994, 0.009296, 0.010735999999999999, 0.009519999999999999, 0.010607999999999998, 0.009296000000000004, 0.009743999999999997, 0.011535999999999998, 0.007503999999999999, 0.009648000000000004, 0.010384, 0.012912000000000003, 0.009968000000000006, 0.009776, 0.008272000000000007, 0.009039999999999998, 0.010767999999999998, 0.007439999999999999, 0.007248000000000001, 0.010480000000000003, 0.009936000000000004, 0.010095999999999996, 0.008784, 0.011824, 0.007503999999999999, 0.007983999999999996, 0.006576000000000004, 0.014159999999999999, 0.010224000000000002, 0.010127999999999996, 0.008719999999999997, 0.009712, 0.009903999999999996, 0.009967999999999994, 0.008144000000000005, 0.008495999999999997, 0.008975999999999994, 0.008368000000000007, 0.010928, 0.009616000000000003, 0.011375999999999999, 0.009232000000000004, 0.012143999999999999, 0.009456, 0.008207999999999998, 0.011760000000000003, 0.009999999999999998, 0.007824000000000003, 0.011215999999999999, 0.009040000000000003, 0.009168, 0.010864, 0.007824, 0.009968000000000003, 0.010256000000000001, 0.008879999999999997, 0.008848, 0.009871999999999995, 0.009680000000000001, 0.012240000000000003, 0.014544000000000001, 0.01064, 0.009487999999999998, 0.008208000000000002, 0.009135999999999998, 0.008975999999999998, 0.008624, 0.009264000000000001, 0.009263999999999998, 0.011375999999999999, 0.008208, 0.009456, 0.009232000000000004, 0.011120000000000001, 0.010480000000000005, 0.013263999999999998, 0.013807999999999997, 0.007375999999999996, 0.009775999999999996, 0.008815999999999997, 0.011599999999999997, 0.010256000000000001, 0.007664, 0.008848000000000002, 0.00856, 0.010575999999999997, 0.011503999999999992, 0.011503999999999993, 0.011119999999999993, 0.009103999999999998, 0.008624, 0.007312000000000002, 0.011823999999999996, 0.009488000000000002, 0.010672, 0.010000000000000002, 0.009967999999999994, 0.010607999999999992, 0.010384000000000003, 0.008944, 0.010032000000000001, 0.008815999999999997, 0.009488, 0.008911999999999998, 0.0074719999999999995, 0.011152, 0.009807999999999997, 0.009232, 0.009936, 0.010863999999999999, 0.011663999999999994, 0.009775999999999993, 0.008143999999999997, 0.011088000000000002, 0.010735999999999999, 0.009711999999999993, 0.010352000000000002, 0.009552000000000003, 0.009711999999999998, 0.011343999999999998, 0.009840000000000002, 0.009296, 0.012623999999999998, 0.009263999999999993, 0.010896000000000003, 0.009168, 0.010095999999999999, 0.009199999999999998, 0.008752, 0.008463999999999998, 0.010352000000000002]
		Thresh_rmse = [0.012325402346561437, 0.008648578293322636, 0.01768466505647737, 0.02417812765770198, 0.01496784076605531, 0.012669928831850004, 0.01652059547407476, 0.017785968808595697, 0.01469006085795644, 0.0173880668030377, 0.021772308134547527, 0.013668159564607376, 0.02268432921392568, 0.014642321340002111, 0.01725791346287817, 0.013598806415944385, 0.023951121779966722, 0.01854282162575935, 0.017423033465824217, 0.023952919503622404, 0.02211508977128628, 0.024894087184245094, 0.016219560477590534, 0.028333663003802894, 0.02082106604270338, 0.016049713991918554, 0.02258877314034665, 0.018504248638430504, 0.022924612990874893, 0.03162608480542159, 0.019454582575820334, 0.015042288382014268, 0.02099319175885582, 0.02198361261487195, 0.02105645352909519, 0.025247935702469325, 0.01903909695171564, 0.019171791242985656, 0.02232829160490971, 0.020492639250329545, 0.023027705282668016, 0.027593088537352175, 0.02082928396084525, 0.021910305057699052, 0.026165307976316127, 0.021622414233641304, 0.030508006495430496, 0.027447729175703976, 0.019535426688218623, 0.029920189148893148, 0.025798470871241267, 0.02726150578797255, 0.024549423497817267, 0.03254160087933306, 0.024255409094403175, 0.021740370998775783, 0.02327416292003626, 0.03270538724002642, 0.02002488549755776, 0.02250188167987932, 0.02565430147151727, 0.02996550665907619, 0.023826651303642202, 0.021263645086051434, 0.027047987747861272, 0.01952539677149377, 0.028905455098061777, 0.0295884506547069, 0.02859862956763467, 0.02675051216353153, 0.02763166631989619, 0.029926918707315946, 0.02775964313347404, 0.03191526027917266, 0.03244498291090838, 0.026862216873663258, 0.0254015055798046, 0.03315042340357417, 0.03427644319799395, 0.030620166672085402, 0.03581635884756869, 0.03016371136526605, 0.027270497374288626, 0.04352485248894631, 0.030806132993729855, 0.02835446833727121, 0.032239637171603226, 0.029373086414414384, 0.03215554466629694, 0.03502984925846041, 0.04517346322475298, 0.03653752123317145, 0.030174326891494893, 0.032712147804724014, 0.034748150929609666, 0.030453943165694657, 0.03373070262354314, 0.0350284068094863, 0.03442825973392395, 0.04382088932806745, 0.033664692074651024, 0.03341790320441976, 0.03785416341351851, 0.03207614695423697, 0.0376289783121913, 0.02780160514762358, 0.04067700829140331, 0.027276207110046368, 0.031136753308730506, 0.03260680849961067, 0.029998119586364985, 0.03695101627493004, 0.03714166989208354, 0.033847729664115346, 0.02984344395750868, 0.035574074555398884, 0.03683560144302537, 0.04199518514471663, 0.04412926833239046, 0.03891131938650899, 0.034687145641572284, 0.035774427645530744, 0.03590666048877984, 0.040985964406054363, 0.0395339071951426, 0.033518249930246664, 0.03624048521170861, 0.035020953474042764, 0.03389094410469177, 0.04545106608216412, 0.04031727578301494, 0.03587807249058029, 0.04104721957679537, 0.03734843246126351, 0.0353223055351906, 0.036958857599637476, 0.05417718065808885, 0.044375377234887055, 0.040993532792784115, 0.05002425958297549, 0.0394387679208713, 0.03821705395899506, 0.04542983192822791, 0.04557202305743714, 0.04202727619405675, 0.04312158007218805, 0.037912582392426716, 0.03806136639147773, 0.05044822790254523, 0.043711939021649486, 0.04483100479114901, 0.03867548540413343, 0.04306286263331652, 0.04591106492588295, 0.05699044466833256, 0.05513191608906506, 0.046538999281766, 0.04937179625267114, 0.04705626230061939, 0.043163824385053624, 0.043425113577350107, 0.04649693741188496, 0.04442159252558652, 0.043079520478646306, 0.04490531314620271, 0.049855952302891735, 0.046434370482354145, 0.0425436349839823, 0.046967226622538584, 0.04065599803263262, 0.04749718237910571, 0.04300090936618399, 0.04480852852963959, 0.0441393395937384, 0.05171091220052698, 0.05425267906624876, 0.04903230820629056, 0.04206223252996028, 0.04834417322109559, 0.05442654220220173, 0.04558536927108353, 0.05442734575252588, 0.04409736243078586, 0.051301074102143754, 0.05282071436063009, 0.05622933396291566, 0.04886322200623333, 0.04811, 0.0731370665845353, 0.05212614001903247, 0.035163581255424, 0.045200381419635914, 0.06800961761737286, 0.056025470275899616, 0.06488651688914762, 0.03928512652741694, 0.04920939488758404, 0.066741834697934, 0.049206156937237615, 0.05371974687212744]

		Gauss_rmse = [0.0019106994553956505, 0.002598408208777066, 0.0033912669637466077, 0.0036836949536411734, 0.003970144564349022, 0.005234914658683905, 0.006531725119565338, 0.006253084700517541, 0.006383704226369628, 0.007538543730732342, 0.008890576246901783, 0.007789329271283913, 0.006871078640291508, 0.008910203904191567, 0.009081821697198744, 0.01011253531479696, 0.010808247251221188, 0.011110092925658479, 0.011483925750743331, 0.012573396998964663, 0.01248559190382715, 0.01405446411033435, 0.013663715058024422, 0.014631985021413843, 0.015342084963550053, 0.0156366657067024, 0.01595968606932879, 0.016204349238859554, 0.018535883760037272, 0.017436003516188597, 0.018315064019379417, 0.018181666496593415, 0.020991284468527275, 0.020914180703094684, 0.020304171527634793, 0.02240233926442992, 0.022363625300859037, 0.023151282238163717, 0.023978952908939536, 0.023692735530293007, 0.02320193459712185, 0.02590640796688815, 0.023413310455052398, 0.02441479729418846, 0.025652917213043608, 0.025704118776535462, 0.02696487432918261, 0.026901660085939083, 0.02636700804648556, 0.027179249760373447, 0.02832437556886153, 0.029239910471609253, 0.027944389889270296, 0.02963733562566715, 0.031156505397884438, 0.0312898319181511, 0.03440808798138748, 0.03307303827675884, 0.03228913325938815, 0.03255768002821487, 0.03287916979195771, 0.03466709886605821, 0.034315840005599965, 0.03588032303355635, 0.035507522832875754, 0.03870432473463683, 0.03378751627481747, 0.03556547968136645, 0.038632931480817055, 0.03906033520470761, 0.03849563365083741, 0.039747974671270614, 0.040278137991849844, 0.04029152198873929, 0.0402250088797146, 0.03986140229406467, 0.04337880884655256, 0.04158451353743922, 0.04065129563777843, 0.04170797304157231, 0.043760714337367626, 0.043292656961259286, 0.042880030942606145, 0.043721081188805314, 0.04518158580601367, 0.04597282798081917, 0.04435467741655897, 0.04486522861606826, 0.04782458925994861, 0.04733445151726109, 0.048260807251703106, 0.047523300225254944, 0.049070849551072354, 0.0486351138486523, 0.05436046798036134, 0.049518056768499144, 0.05100850400008153, 0.05348849853556179, 0.05226257670988414, 0.05026561431901607, 0.053228890881919504, 0.052713729576039745, 0.05308441561061574, 0.05437913498270084, 0.056465092441539105, 0.05422279859338211, 0.05565712314330718, 0.05318583149591428, 0.05659067996311713, 0.05752439803379577, 0.05601989527089565, 0.057419548051111616, 0.056225992515552096, 0.05851916135359163, 0.05990251537383781, 0.05654584580017992, 0.05979696399738812, 0.059660413963637596, 0.060318288134203736, 0.062103154116036306, 0.06219846352783635, 0.06086569226119796, 0.06294030456359115, 0.0637938411484959, 0.06443684292589072, 0.06294038967404456, 0.06270326468569663, 0.06429833175735862, 0.063821755690605, 0.06384481854878758, 0.06491174461931185, 0.06651333027433187, 0.06669993063290652, 0.06756775559785862, 0.06460969360359296, 0.0647893184545494, 0.06737137160545222, 0.06953665610725633, 0.06815413106425824, 0.06883783839441145, 0.0708534519540023, 0.06947492505646691, 0.06979548905535161, 0.07294905298602399, 0.07257091273504127, 0.06917888722750071, 0.07386389562946227, 0.07416264856358479, 0.07410319971169597, 0.07167766585171556, 0.07389767366757549, 0.07564677323568712, 0.07711006873847176, 0.07690137354283634, 0.07212892080226217, 0.07292046427272841, 0.07640222494668483, 0.07569470557492866, 0.07836407939624282, 0.08388519333655185, 0.07837988355114021, 0.08125243365105353, 0.07707935691080996, 0.08082580521241774, 0.07939192924003048, 0.0802479475765499, 0.08144006651425519, 0.0815749630834665, 0.08170714987091805, 0.0818227504953754, 0.08334624568864318, 0.08601442601698739, 0.0817508317765711, 0.08297882584064911, 0.08577633442434486, 0.08368899680178801, 0.08446067145499488, 0.08620343293129175, 0.08523564045318896, 0.08474697173149037, 0.0843838195234526, 0.08500015556411875, 0.0890542546195172, 0.08458344237129238, 0.0869129028960955, 0.08912158754802599, 0.0890612345350702, 0.08791659975172438, 0.08858541931352887, 0.08858882394547266, 0.08822711849345748, 0.09311277935246237, 0.09176360854451769, 0.09065169047457386, 0.0915520088262352, 0.09195302261865686, 0.0894070266956672, 0.09230165738626203, 0.09356347418848557, 0.09300131758362705]

		GnC_gauss_rmse = [0.0019761858448707923, 0.0024572782938830917, 0.0029534335694922764, 0.003465562863442979, 0.0045025192216988415, 0.0044673417860914034, 0.005288908949632341, 0.007306728359572794, 0.00637768239110639, 0.006449932890595006, 0.007118649833306192, 0.008304542352128508, 0.008442578816967068, 0.008780284040208726, 0.008973122581267493, 0.011477640870774521, 0.010972172441494404, 0.011330243250391442, 0.011768417116574973, 0.011641250588858456, 0.012136682685244699, 0.01311230290705135, 0.012476396466131655, 0.01349350747216856, 0.015988938260990956, 0.015259355557396291, 0.013969800699873788, 0.017340976759149234, 0.01691410356228527, 0.017455313685247786, 0.01822930483404921, 0.018549571642551343, 0.01945538316881513, 0.02053362864359729, 0.0202071322134067, 0.02299025304945176, 0.019624619517832956, 0.023002131181857587, 0.02304172256937062, 0.022836203627478117, 0.023734826165297474, 0.02363986910961703, 0.025252386253815397, 0.026040740283777876, 0.025842507509476077, 0.02606107120177858, 0.028482085961587397, 0.02733558477035606, 0.027339021195050685, 0.028134372791191575, 0.02807201278997914, 0.029741588112695116, 0.02999154356148208, 0.03100151413317626, 0.030479366381766765, 0.031059772190105462, 0.03424129340777137, 0.034244901207992604, 0.033564437088024854, 0.03521968022652547, 0.03578701948131155, 0.03560853919244638, 0.0369601465043904, 0.03521616211225729, 0.03686581893032983, 0.03594326727657911, 0.039637845374359916, 0.03766587386792504, 0.03801592461938577, 0.040480629478303436, 0.03911019035079617, 0.03946315248281474, 0.04027834763938767, 0.04093341747760392, 0.0395927940082915, 0.041442241589954756, 0.040267338040081, 0.04614168173684629, 0.04113543824154671, 0.042256889044474645, 0.04393274217404718, 0.04323899464720467, 0.04587008793914326, 0.04496155135820787, 0.04657321652949276, 0.04797602394997384, 0.04629856358069029, 0.04725269600788153, 0.04750992846802833, 0.047570039357354856, 0.05042244221592802, 0.04661633011960152, 0.04946350985741968, 0.047489527873902565, 0.04866901988515277, 0.05007412105159702, 0.051719761346144456, 0.05237641484090549, 0.05368287826869934, 0.054334877955490135, 0.051693899203971376, 0.05252137376050336, 0.05305080221925891, 0.05271699024876697, 0.05271875016213432, 0.05603259417777834, 0.05496753774052658, 0.05558198475223608, 0.060104564924428124, 0.05569458918756646, 0.05783188385598967, 0.05828231851783675, 0.05945650477485356, 0.0586593801274177, 0.057786981467952664, 0.060564538263576564, 0.058810231997947195, 0.057035004664142706, 0.06036798456581252, 0.059943565323409366, 0.0612340285498138, 0.062110659896499706, 0.06268767916848077, 0.06366253742202137, 0.06278098095822776, 0.06446044671058371, 0.06528932488937342, 0.06427536171550065, 0.06478164370603162, 0.06516607737051865, 0.06713877720577954, 0.06587012635133153, 0.06679699583815549, 0.06984084273214025, 0.06617787998157273, 0.06877913174614378, 0.06781799572961614, 0.06985071862422557, 0.06801949126818094, 0.07120888338161763, 0.07455022456833456, 0.07131633617603989, 0.0734052153151202, 0.07053735907953693, 0.07167986804908635, 0.06965248701770291, 0.07401684099916002, 0.07196624063531823, 0.07491614642516556, 0.0737427944745753, 0.0764802483721207, 0.07508017944397978, 0.07396253775383137, 0.0769944787977954, 0.07807904388625098, 0.07642601474791777, 0.08025310563338588, 0.0787660917897848, 0.07707093280326678, 0.08148348201451835, 0.07871405661358981, 0.08303434315317099, 0.078775207886635, 0.07850050249121497, 0.08230326032799669, 0.08148145613005467, 0.08346573788775827, 0.08309351162244162, 0.0850693446894775, 0.08467355056297395, 0.0850070477741212, 0.08526027955281995, 0.0823120988381035, 0.08469236718714436, 0.0870725150462998, 0.08559999239611205, 0.08755031021716331, 0.08385221217037765, 0.08653743770870687, 0.08763099282917834, 0.08526733149066855, 0.08606636253131672, 0.08601410198413767, 0.08781148689228661, 0.0849702278831093, 0.08672495414501963, 0.08763979108897878, 0.08741629310224391, 0.09055188064771502, 0.08409896394776704, 0.08605199171627267, 0.08406600870949284, 0.08683915027692295, 0.08615359676734811, 0.09400094087275188, 0.08971149767088067, 0.09013985573241615, 0.09452900136889367, 0.0970321885339966, 0.09493446344660297]

		GnC_thresh_rmse = [0.022029166959202096, 0.02561806754098691, 0.009741609487851428, 0.007074677347050048, 0.017886148161202226, 0.01622652650607782, 0.0206286128329196, 0.014315057271097506, 0.0348761488940303, 0.006036358009698103, 0.02020346251844571, 0.01746655166845722, 0.017383893881234302, 0.01848836350072684, 0.01292554298846291, 0.0201812386067565, 0.020975893666520216, 0.012967524416058553, 0.022519530495515062, 0.026913167570416268, 0.020450276829349145, 0.017702738499072893, 0.02671634449293952, 0.013501666204297484, 0.011933084991040803, 0.02093978901206641, 0.016848734464123345, 0.02659606344419423, 0.016997710385496034, 0.025291529257321203, 0.026538202167682705, 0.02350853292091279, 0.016559160577123543, 0.019591397453214567, 0.022854369389456636, 0.02682800971595918, 0.013015314515543502, 0.020260378702526366, 0.023311961717260413, 0.01592699526191907, 0.05395235081505728, 0.024973503570042598, 0.02414948816695031, 0.028275779094096253, 0.022466097005856386, 0.01671004081997814, 0.025116299222260885, 0.02426348677865193, 0.02756934751338843, 0.03533561454167497, 0.026102609468604862, 0.026843173110321587, 0.023724514081440783, 0.023327936234238636, 0.02315114742692851, 0.023269328713084392, 0.024889799038486517, 0.030206466822735204, 0.03188879215905377, 0.02748054993959304, 0.01820947554853544, 0.02773117019559856, 0.019464201278008617, 0.025083428719661363, 0.026006841535864283, 0.02718846742015886, 0.03769627292015652, 0.026710830301753558, 0.02621681097539922, 0.02662208699128099, 0.02681218945346803, 0.030415672096952732, 0.029261892872525914, 0.03512010480823971, 0.019588222601254902, 0.031883974718836625, 0.020356377767423193, 0.028448727999448073, 0.026458444137079408, 0.039727093391013095, 0.020924392622516903, 0.025772264809508127, 0.03501120047516433, 0.023825851984491678, 0.024776997591722506, 0.026114497215254676, 0.031060841510157967, 0.030838249072766655, 0.0382746877106718, 0.025962062696835698, 0.02606641554499676, 0.03291026640975467, 0.025135140199934836, 0.022384146902435145, 0.023652296525841447, 0.02701226668825127, 0.03646962500280307, 0.03821902389704108, 0.030048949404051433, 0.028210154366648442, 0.030163011670567584, 0.03765184643889699, 0.029627960937890725, 0.028287071908389604, 0.0311890224222371, 0.024446287034759834, 0.03743817244251193, 0.04286491776043897, 0.030846294643790483, 0.0330679386056835, 0.03331500657900095, 0.023661821599774622, 0.029457969555869587, 0.04056451977187524, 0.024781740888295944, 0.03052315918473357, 0.03917187446206014, 0.03972590992904115, 0.03538429482166627, 0.0450888621968596, 0.02745582725747783, 0.025660646749470467, 0.03891115300405381, 0.03244978058055161, 0.032091717434294655, 0.032513201226038645, 0.03986663313549792, 0.03553499089428321, 0.04303746319620738, 0.0463953546397762, 0.03135873362929329, 0.039143123136665514, 0.03106694205015283, 0.04178354396992354, 0.03961119436250502, 0.04036173949156887, 0.033959063342230957, 0.031173820746698513, 0.034464649038740476, 0.03777521878560006, 0.04738879256678019, 0.03728092748941934, 0.045498299257417205, 0.04009100668051187, 0.036805005221104546, 0.04378293179828972, 0.04759796187453777, 0.03784379407431914, 0.04149021719827378, 0.04901318165592465, 0.03762370130186239, 0.04569569124042063, 0.03872732904818865, 0.05185642777475022, 0.039113489157403224, 0.04884948540838144, 0.04346766866761083, 0.044221763008013434, 0.04227244220256249, 0.050227374582188615, 0.03656102163542018, 0.04323740502014292, 0.045714895803431084, 0.0515473791252919, 0.04034514970483745, 0.051005704888209095, 0.045570850491963785, 0.04333558378727876, 0.03938510355747394, 0.04385182359931902, 0.03743426076327176, 0.039078155990699276, 0.044273473675046464, 0.04919751406553843, 0.05086170023706385, 0.048184288593262695, 0.04556411432226243, 0.04598784322851375, 0.04068911219811509, 0.042435037592265544, 0.04505114653346753, 0.036411738251031424, 0.03960922827344876, 0.042714480859522404, 0.04942903037109632, 0.04697177118861084, 0.056081896600077445, 0.04851675925717264, 0.046140336683384284, 0.049631315813851946, 0.058121284215492564, 0.039232325642228866, 0.040405296020857115, 0.04416308942791341, 0.04505753913685134, 0.051966835051866535, 0.05043913211392371, 0.05012980760116842, 0.03989270326058776, 0.04363657214811086]

		GnC_DataSplit_rmse = [0.008677685950413221, 0.011033057851239668, 0.00975206611570248, 0.01177685950413223, 0.00983471074380165, 0.018471074380165292, 0.009338842975206606, 0.00995867768595041, 0.013388429752066111, 0.012644628099173556, 0.011280991735537187, 0.01099173553719008, 0.01136363636363636, 0.010950413223140498, 0.009421487603305783, 0.0143801652892562, 0.007768595041322312, 0.012520661157024795, 0.013842975206611572, 0.009214876033057848, 0.01165289256198347, 0.012644628099173556, 0.01227272727272727, 0.010289256198347103, 0.010454545454545452, 0.012603305785123966, 0.012107438016528922, 0.01161157024793388, 0.011322314049586779, 0.015041322314049593, 0.008636363636363635, 0.010289256198347103, 0.009710743801652892, 0.010495867768595039, 0.009297520661157023, 0.012107438016528922, 0.009380165289256193, 0.009999999999999998, 0.01276859504132232, 0.014793388429752063, 0.010867768595041318, 0.009090909090909087, 0.01260330578512397, 0.010950413223140497, 0.015619834710743801, 0.01037190082644628, 0.009049586776859502, 0.013966942148760333, 0.014173553719008265, 0.01351239669421488, 0.010826446280991733, 0.009958677685950408, 0.013057851239669422, 0.009421487603305783, 0.008099173553719004, 0.009876033057851235, 0.006735537190082643, 0.011487603305785124, 0.010041322314049583, 0.012685950413223138, 0.008057851239669421, 0.010537190082644622, 0.010413223140495864, 0.010330578512396693, 0.013181818181818182, 0.014421487603305784, 0.010123966942148756, 0.007272727272727273, 0.010867768595041323, 0.010785123966942147, 0.012933884297520664, 0.011611570247933882, 0.010661157024793387, 0.014008264462809915, 0.011983471074380162, 0.010950413223140491, 0.008677685950413218, 0.013099173553719012, 0.009876033057851235, 0.015330578512396697, 0.012355371900826442, 0.011652892561983468, 0.008512396694214875, 0.0096694214876033, 0.011239669421487604, 0.012479338842975207, 0.015743801652892565, 0.01140495867768595, 0.011033057851239668, 0.012148760330578507, 0.01107438016528926, 0.010330578512396691, 0.017396694214876034, 0.011528925619834708, 0.01202479338842975, 0.014917355371900825, 0.01636363636363636, 0.012851239669421486, 0.014628099173553719, 0.015247933884297526, 0.009793388429752062, 0.012933884297520664, 0.007851239669421486, 0.014090909090909083, 0.0109504132231405, 0.010785123966942152, 0.011900826446280993, 0.011198347107438014, 0.01099173553719008, 0.009710743801652887, 0.007644628099173553, 0.01371900826446281, 0.012438016528925622, 0.011446280991735537, 0.006446280991735534, 0.01161157024793388, 0.008305785123966937, 0.011115702479338838, 0.012851239669421484, 0.009917355371900822, 0.007975206611570243, 0.011942148760330576, 0.010454545454545447, 0.011983471074380162, 0.009008264462809912, 0.014214876033057851, 0.009090909090909089, 0.007851239669421483, 0.006157024793388428, 0.01537190082644628, 0.01057851239669421, 0.009462809917355368, 0.016322314049586777, 0.012148760330578514, 0.009173553719008264, 0.00855371900826446, 0.009132231404958675, 0.010619834710743799, 0.010206611570247935, 0.011776859504132231, 0.00921487603305785, 0.007520661157024789, 0.009752066115702476, 0.009958677685950412, 0.008760330578512394, 0.014008264462809924, 0.015165289256198344, 0.011570247933884302, 0.011363636363636367, 0.01165289256198347, 0.010165289256198345, 0.011570247933884297, 0.013140495867768602, 0.009628099173553718, 0.01037190082644628, 0.010041322314049585, 0.010495867768595039, 0.009710743801652889, 0.011528925619834708, 0.009214876033057847, 0.01074380165289256, 0.013099173553719005, 0.013636363636363636, 0.010785123966942147, 0.0073140495867768576, 0.014917355371900823, 0.013305785123966941, 0.010702479338842974, 0.012438016528925617, 0.009380165289256195, 0.010206611570247935, 0.009256198347107433, 0.013181818181818182, 0.012272727272727274, 0.009876033057851237, 0.009380165289256198, 0.012851239669421491, 0.010413223140495862, 0.006652892561983468, 0.016487603305785126, 0.014669421487603307, 0.013719008264462809, 0.010991735537190081, 0.013223140495867773, 0.015595463137996219, 0.00803402646502836, 0.008034026465028358, 0.0012287334593572763, 0.0050094517958412105, 0.021644612476370513, 0.017107750472589795, 0.007277882797731566, 0.0027410207939508493, 0.0042533081285444215, 0.011058601134215498, 0.008034026465028354, 0.009546313799621932, 0.008034026465028352, 0.005765595463137998, 0.007277882797731572]

		l = min(len(Baseline_rmse), len(DataSplit_rmse), len(Thresh_rmse), len(Gauss_rmse), len(GnC_gauss_rmse), len(GnC_thresh_rmse), len(GnC_DataSplit_rmse))
		x_list = range(10, 2001, 10)[:l]
		plt.plot(x_list, Baseline_rmse[:l], 'r', label= "Baseline")
		plt.plot(x_list, DataSplit_rmse[:l], 'y', label= "DataSplit")
		plt.plot(x_list, Thresh_rmse[:l], 'g', label= "Thresh")
		plt.plot(x_list, Gauss_rmse[:l], 'b', label= "Gauss")
		plt.plot(x_list, GnC_gauss_rmse[:l], 'm', label= "GnC_gauss")
		plt.plot(x_list, GnC_thresh_rmse[:l], 'c', label= "GnC_thresh")
		plt.plot(x_list, GnC_DataSplit_rmse[:l], label= "GnC_DataSplit")
		plt.xlabel("Queries")
		plt.ylabel("RMSE (Generalization Error) for adaptive queries")
		plt.legend()
		plt.grid()
		plt.savefig("../plots/combined.png")
		plt.show()


r = AdaptPlot()
r.plot_from_data()


		